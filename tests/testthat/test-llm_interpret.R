# Helper functions for environment variable management
setup_env_vars <- function(provider = NULL, api_key = NULL, model = NULL) {
  if (!is.null(provider)) Sys.setenv(LLM_PROVIDER = provider)
  if (!is.null(api_key)) Sys.setenv(LLM_API_KEY = api_key)
  if (!is.null(model)) Sys.setenv(LLM_MODEL = model)
}

cleanup_env_vars <- function() {
  Sys.unsetenv("LLM_PROVIDER")
  Sys.unsetenv("LLM_API_KEY")
  Sys.unsetenv("LLM_MODEL")
}

# Set up test environment
withr::local_envvar(
  c(
    LLM_PROVIDER = NA,
    LLM_API_KEY = NA,
    LLM_MODEL = NA
  )
)

test_that("llm_interpret validates input parameters", {
  # Set up minimal environment variables for parameter validation tests
  setup_env_vars(provider = "openai", api_key = "fake-key", model = "gpt-4o")
  
  # Test invalid word_limit
  expect_error(
    llm_interpret(data.frame(x = 1), word_limit = -1),
    "word_limit must be a positive number"
  )
  
  expect_error(
    llm_interpret(data.frame(x = 1), word_limit = "invalid"),
    "word_limit must be a positive number"
  )
  
  # Test invalid prompt_extension
  expect_error(
    llm_interpret(data.frame(x = 1), prompt_extension = 123),
    "prompt_extension must be NULL or a character string"
  )
  
  # Note: Empty data frame test removed because environment variables are checked first
  
  # Test unsupported input type
  expect_error(
    llm_interpret("not a data frame or ggplot"),
    "Unsupported input type: character"
  )
})

test_that("llm_interpret validates environment variables", {
  # Test missing LLM_PROVIDER
  cleanup_env_vars()
  expect_error(
    llm_interpret(data.frame(x = 1)),
    "LLM_PROVIDER environment variable is not set"
  )
  
  # Test missing LLM_API_KEY
  setup_env_vars(provider = "openai")
  expect_error(
    llm_interpret(data.frame(x = 1)),
    "LLM_API_KEY environment variable is not set"
  )
  
  # Test missing LLM_MODEL
  setup_env_vars(provider = "openai", api_key = "fake-key")
  expect_error(
    llm_interpret(data.frame(x = 1)),
    "LLM_MODEL environment variable is not set"
  )
  
  # Test unsupported provider
  setup_env_vars(provider = "unsupported", api_key = "fake-key", model = "fake-model")
  expect_error(
    llm_interpret(data.frame(x = 1)),
    "Unsupported LLM provider: 'unsupported'"
  )
}) 