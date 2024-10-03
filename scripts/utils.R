

# Function to play nice with the API and send requests in chunks
get_embeddings = function(model, text,...){
  N=length(text)
  
  if(N<=2000){
    e1 <- create_embedding( model = model, input = text, ...)
    embed = do.call(rbind,e1$data$embedding)
    rm(e1)
  }
  else if (N>2000){
  e1 <- create_embedding( model = model, input = text[1:2000], ...)
  embed = do.call(rbind,e1$data$embedding)
  rm(e1)
  
  start = 2001
  end = 4000
  
  
  while (nrow(embed)<N){
    print(c(start,end))
    if (nrow(embed)==N){break}
    emb = create_embedding(model =  model, input = text[start:end], ...)
    
    emb.out = do.call(rbind, emb$data$embedding)
    embed = rbind(embed, emb.out)
    rm(emb, emb.out)
    
    start = start+2000
    end = min(end+2000,N)
    
  }
  }
  
  return(embed)
  
}


create_embedding<- function(
    engine_id = lifecycle::deprecated(),
    model,
    input,
    user = NULL,
    dimensions=NULL,
    openai_api_key = Sys.getenv("OPENAI_API_KEY"),
    openai_organization = NULL
) {
  
  if (lifecycle::is_present(engine_id)) {
    lifecycle::deprecate_warn(
      "0.3.0",
      "create_completion(engine_id)",
      "create_completion(model)"
    )
    model <- engine_id
  }
  
  #---------------------------------------------------------------------------
  # Validate arguments
  
  assertthat::assert_that(
    assertthat::is.string(model),
    assertthat::noNA(model)
  )
  
  assertthat::assert_that(
    is.character(input),
    assertthat::noNA(input)
  )
  
  if (!is.null(user)) {
    assertthat::assert_that(
      assertthat::is.string(user),
      assertthat::noNA(user)
    )
  }
  
  assertthat::assert_that(
    assertthat::is.string(openai_api_key),
    assertthat::noNA(openai_api_key)
  )
  
  if (!is.null(openai_organization)) {
    assertthat::assert_that(
      assertthat::is.string(openai_organization),
      assertthat::noNA(openai_organization)
    )
  }
  
  #---------------------------------------------------------------------------
  # Build path parameters
  
  task <- "embeddings"
  
  base_url <- glue::glue("https://api.openai.com/v1/{task}")
  
  headers <- c(
    "Authorization" = paste("Bearer", openai_api_key),
    "Content-Type" = "application/json"
  )
  
  if (!is.null(openai_organization)) {
    headers["OpenAI-Organization"] <- openai_organization
  }
  
  #---------------------------------------------------------------------------
  # Build request body
  
  body <- list()
  body[["model"]] <- model
  body[["input"]] <- input
  body[["user"]] <- user
  body[["dimensions"]] <- dimensions
  
  #---------------------------------------------------------------------------
  # Make a request and parse it
  
  response <- httr::POST(
    url = base_url,
    httr::add_headers(.headers = headers),
    body = body,
    encode = "json"
  )
  
  verify_mime_type(response)
  
  parsed <- response %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
  
  #---------------------------------------------------------------------------
  # Check whether request failed and return parsed
  
  if (httr::http_error(response)) {
    paste0(
      "OpenAI API request failed [",
      httr::status_code(response),
      "]:\n\n",
      parsed$error$message
    ) %>%
      stop(call. = FALSE)
  }
  
  parsed
  
}


verify_mime_type <- function(result) {
  
  if (httr::http_type(result) != "application/json") {
    paste(
      "OpenAI API probably has been changed. If you see this, please",
      "rise an issue at: https://github.com/irudnyts/openai/issues"
    ) %>%
      stop()
  }
  
}
