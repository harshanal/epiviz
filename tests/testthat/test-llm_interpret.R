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

test_that("llm_interpret works with different providers", {
  # Mock all LLM clients to return a test response
  mock_client <- function(...) {
    list(
      set_system_prompt = function(...) NULL,
      chat = function(...) "Test response"
    )
  }
  
  mockery::stub(llm_interpret, "chat_openai", mock_client)
  mockery::stub(llm_interpret, "chat_gemini", mock_client)
  mockery::stub(llm_interpret, "chat_claude", mock_client)
  
  test_input <- data.frame(x = 1:3, y = letters[1:3])
  
  # Test OpenAI
  setup_env_vars(
    provider = "openai",
    api_key = "fake-key",
    model = "gpt-4o"
  )
  expect_equal(
    llm_interpret(test_input),
    "Test response"
  )
  
  # Test Gemini
  setup_env_vars(
    provider = "gemini",
    api_key = "fake-key",
    model = "gemini-1.5-flash"
  )
  expect_equal(
    llm_interpret(test_input),
    "Test response"
  )
  
  # Test Claude
  setup_env_vars(
    provider = "claude",
    api_key = "fake-key",
    model = "claude-1"
  )
  expect_equal(
    llm_interpret(test_input),
    "Test response"
  )
}) 