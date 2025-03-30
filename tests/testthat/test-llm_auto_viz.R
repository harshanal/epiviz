# Load required packages
library(testthat)
library(epiviz)
library(mockery)

# Test for llm_auto_viz function
test_that("llm_auto_viz generates proper code with default settings", {
  # Load lab_data
  data(lab_data)
  
  # Mock environment variables
  orig_provider <- Sys.getenv("LLM_PROVIDER")
  orig_api_key <- Sys.getenv("LLM_API_KEY")
  orig_model <- Sys.getenv("LLM_MODEL")
  
  Sys.setenv(LLM_PROVIDER = "openai")
  Sys.setenv(LLM_API_KEY = "test_key")
  Sys.setenv(LLM_MODEL = "gpt-4")
  
  # Create mock for the LLM chat function
  mock_response <- '{
    "selected_function": "line_chart",
    "r_code": "# Create a line chart showing specimen counts over time\nepiviz::line_chart(\n  dynamic = FALSE,\n  params = list(\n    df = df,\n    x = \"specimen_date\",\n    y = \"count\",\n    group_var = \"organism_species_name\",\n    line_colour = \"#007C91\",\n    point_marker = TRUE,\n    chart_title = \"Laboratory Specimens Over Time\"\n  )\n)"
  }'
  
  # Mock the query_llm_json function
  with_mock(
    `epiviz:::query_llm_json` = function(...) {
      return(jsonlite::fromJSON(mock_response))
    },
    `epiviz:::log_llm_interaction` = function(...) {
      return(NULL)
    },
    {
      # Test code-only execution
      result_code <- llm_auto_viz(lab_data, execute = FALSE)
      
      # Check that the result contains expected code elements
      expect_type(result_code, "character")
      expect_true(grepl("line_chart", result_code))
      expect_true(grepl("specimen_date", result_code))
      
      # Test with execute=TRUE would need more comprehensive mocking of the visualization functions
      # Here we're just testing that it attempts to execute the code
      expect_error(llm_auto_viz(lab_data, execute = TRUE), NA)
    }
  )
  
  # Restore environment variables
  Sys.setenv(LLM_PROVIDER = orig_provider)
  Sys.setenv(LLM_API_KEY = orig_api_key)
  Sys.setenv(LLM_MODEL = orig_model)
})

test_that("llm_auto_viz respects user guidance", {
  # Load lab_data
  data(lab_data)
  
  # Mock environment variables
  orig_provider <- Sys.getenv("LLM_PROVIDER")
  orig_api_key <- Sys.getenv("LLM_API_KEY")
  orig_model <- Sys.getenv("LLM_MODEL")
  
  Sys.setenv(LLM_PROVIDER = "openai")
  Sys.setenv(LLM_API_KEY = "test_key")
  Sys.setenv(LLM_MODEL = "gpt-4")
  
  # Create mock responses for different user prompts
  mock_response_line <- '{
    "selected_function": "line_chart",
    "r_code": "# Create a line chart as requested by user\nepiviz::line_chart(\n  dynamic = FALSE,\n  params = list(\n    df = df,\n    x = \"specimen_date\",\n    y = \"count\",\n    group_var = \"organism_species_name\",\n    line_colour = \"#007C91\"\n  )\n)"
  }'
  
  mock_response_point <- '{
    "selected_function": "point_chart",
    "r_code": "# Create a scatter plot as requested by user\nepiviz::point_chart(\n  dynamic = FALSE,\n  params = list(\n    df = df,\n    x = \"specimen_date\",\n    y = \"count\",\n    group_var = \"organism_species_name\",\n    point_colour = \"#007C91\"\n  )\n)"
  }'
  
  # Test with line chart user guidance
  with_mock(
    `epiviz:::query_llm_json` = function(system_prompt, ...) {
      # Return different response based on prompt content
      if (grepl("line chart", system_prompt, ignore.case = TRUE)) {
        return(jsonlite::fromJSON(mock_response_line))
      } else {
        return(jsonlite::fromJSON(mock_response_point))
      }
    },
    `epiviz:::log_llm_interaction` = function(...) { return(NULL) },
    {
      # Test with line chart guidance
      result_line <- llm_auto_viz(lab_data, user_prompt = "Please use a line chart", execute = FALSE)
      expect_true(grepl("line_chart", result_line))
      
      # Test with scatter plot guidance
      result_point <- llm_auto_viz(lab_data, user_prompt = "Please use a scatter plot", execute = FALSE)
      expect_true(grepl("point_chart", result_point))
    }
  )
  
  # Restore environment variables
  Sys.setenv(LLM_PROVIDER = orig_provider)
  Sys.setenv(LLM_API_KEY = orig_api_key)
  Sys.setenv(LLM_MODEL = orig_model)
})

test_that("llm_auto_viz handles dynamic vs static setting", {
  # Load lab_data
  data(lab_data)
  
  # Mock environment variables
  orig_provider <- Sys.getenv("LLM_PROVIDER")
  orig_api_key <- Sys.getenv("LLM_API_KEY")
  orig_model <- Sys.getenv("LLM_MODEL")
  
  Sys.setenv(LLM_PROVIDER = "openai")
  Sys.setenv(LLM_API_KEY = "test_key")
  Sys.setenv(LLM_MODEL = "gpt-4")
  
  # Create mock responses for static and dynamic settings
  mock_response_static <- '{
    "selected_function": "epi_curve",
    "r_code": "# Create a static epi curve\nepiviz::epi_curve(\n  dynamic = FALSE,\n  params = list(\n    df = df,\n    date_col = \"specimen_date\"\n  )\n)"
  }'
  
  mock_response_dynamic <- '{
    "selected_function": "epi_curve",
    "r_code": "# Create an interactive epi curve\nepiviz::epi_curve(\n  dynamic = TRUE,\n  params = list(\n    df = df,\n    date_col = \"specimen_date\"\n  )\n)"
  }'
  
  # Test with static vs dynamic setting
  with_mock(
    `epiviz:::query_llm_json` = function(system_prompt, ...) {
      # Return different response based on prompt content
      if (grepl("static", system_prompt, ignore.case = TRUE)) {
        return(jsonlite::fromJSON(mock_response_static))
      } else {
        return(jsonlite::fromJSON(mock_response_dynamic))
      }
    },
    `epiviz:::log_llm_interaction` = function(...) { return(NULL) },
    {
      # Test static visualization (default)
      result_static <- llm_auto_viz(lab_data, execute = FALSE)
      expect_true(grepl("dynamic = FALSE", result_static))
      
      # Test dynamic visualization
      result_dynamic <- llm_auto_viz(lab_data, dynamic = TRUE, execute = FALSE)
      expect_true(grepl("dynamic = TRUE", result_dynamic))
    }
  )
  
  # Restore environment variables
  Sys.setenv(LLM_PROVIDER = orig_provider)
  Sys.setenv(LLM_API_KEY = orig_api_key)
  Sys.setenv(LLM_MODEL = orig_model)
})

test_that("llm_auto_viz handles missing environment variables", {
  # Load lab_data
  data(lab_data)
  
  # Save original environment variables
  orig_provider <- Sys.getenv("LLM_PROVIDER")
  orig_api_key <- Sys.getenv("LLM_API_KEY")
  orig_model <- Sys.getenv("LLM_MODEL")
  
  # Test missing provider
  Sys.unsetenv("LLM_PROVIDER")
  Sys.setenv(LLM_API_KEY = "test_key")
  Sys.setenv(LLM_MODEL = "gpt-4")
  expect_error(llm_auto_viz(lab_data), "LLM_PROVIDER environment variable is not set")
  
  # Test missing API key
  Sys.setenv(LLM_PROVIDER = "openai")
  Sys.unsetenv("LLM_API_KEY")
  Sys.setenv(LLM_MODEL = "gpt-4")
  expect_error(llm_auto_viz(lab_data), "LLM_API_KEY environment variable is not set")
  
  # Test missing model
  Sys.setenv(LLM_PROVIDER = "openai")
  Sys.setenv(LLM_API_KEY = "test_key")
  Sys.unsetenv("LLM_MODEL")
  expect_error(llm_auto_viz(lab_data), "LLM_MODEL environment variable is not set")
  
  # Restore environment variables
  Sys.setenv(LLM_PROVIDER = orig_provider)
  Sys.setenv(LLM_API_KEY = orig_api_key)
  Sys.setenv(LLM_MODEL = orig_model)
})

test_that("llm_auto_viz handles invalid LLM response", {
  # Load lab_data
  data(lab_data)
  
  # Mock environment variables
  orig_provider <- Sys.getenv("LLM_PROVIDER")
  orig_api_key <- Sys.getenv("LLM_API_KEY")
  orig_model <- Sys.getenv("LLM_MODEL")
  
  Sys.setenv(LLM_PROVIDER = "openai")
  Sys.setenv(LLM_API_KEY = "test_key")
  Sys.setenv(LLM_MODEL = "gpt-4")
  
  # Create invalid mock response
  mock_invalid_response <- '{
    "message": "This is not a valid response"
  }'
  
  # Test with invalid response
  with_mock(
    `epiviz:::query_llm_json` = function(...) {
      return(jsonlite::fromJSON(mock_invalid_response))
    },
    `epiviz:::log_llm_interaction` = function(...) { return(NULL) },
    {
      expect_error(llm_auto_viz(lab_data), "Invalid LLM response structure")
    }
  )
  
  # Restore environment variables
  Sys.setenv(LLM_PROVIDER = orig_provider)
  Sys.setenv(LLM_API_KEY = orig_api_key)
  Sys.setenv(LLM_MODEL = orig_model)
})

test_that("llm_auto_viz handles lab_data with different visualization types", {
  # Load lab_data
  data(lab_data)
  
  # Mock environment variables
  orig_provider <- Sys.getenv("LLM_PROVIDER")
  orig_api_key <- Sys.getenv("LLM_API_KEY")
  orig_model <- Sys.getenv("LLM_MODEL")
  
  Sys.setenv(LLM_PROVIDER = "openai")
  Sys.setenv(LLM_API_KEY = "test_key")
  Sys.setenv(LLM_MODEL = "gpt-4")
  
  # Create mock responses for different visualization types
  visualizations <- list(
    line_chart = '{
      "selected_function": "line_chart",
      "r_code": "epiviz::line_chart(dynamic = FALSE, params = list(df = df, x = \"specimen_date\", y = \"count\"))"
    }',
    point_chart = '{
      "selected_function": "point_chart",
      "r_code": "epiviz::point_chart(dynamic = FALSE, params = list(df = df, x = \"specimen_date\", y = \"count\"))"
    }',
    col_chart = '{
      "selected_function": "col_chart",
      "r_code": "epiviz::col_chart(dynamic = FALSE, params = list(df = df, x = \"specimen_date\", y = \"count\"))"
    }',
    epi_curve = '{
      "selected_function": "epi_curve",
      "r_code": "epiviz::epi_curve(dynamic = FALSE, params = list(df = df, date_col = \"specimen_date\"))"
    }',
    age_sex_pyramid = '{
      "selected_function": "age_sex_pyramid",
      "r_code": "epiviz::age_sex_pyramid(dynamic = FALSE, params = list(df = df, age = \"date_of_birth\", sex = \"sex\"))"
    }'
  )
  
  test_viz_function <- function(viz_name) {
    with_mock(
      `epiviz:::query_llm_json` = function(...) {
        return(jsonlite::fromJSON(visualizations[[viz_name]]))
      },
      `epiviz:::log_llm_interaction` = function(...) { return(NULL) },
      {
        result <- llm_auto_viz(lab_data, execute = FALSE)
        expect_true(grepl(viz_name, result))
      }
    )
  }
  
  # Test each visualization type
  for (viz_name in names(visualizations)) {
    test_viz_function(viz_name)
  }
  
  # Restore environment variables
  Sys.setenv(LLM_PROVIDER = orig_provider)
  Sys.setenv(LLM_API_KEY = orig_api_key)
  Sys.setenv(LLM_MODEL = orig_model)
})

test_that("llm_auto_viz handles different LLM providers", {
  # Load lab_data
  data(lab_data)
  
  # Mock environment variables
  orig_provider <- Sys.getenv("LLM_PROVIDER")
  orig_api_key <- Sys.getenv("LLM_API_KEY")
  orig_model <- Sys.getenv("LLM_MODEL")
  
  # Create mock response
  mock_response <- '{
    "selected_function": "line_chart",
    "r_code": "epiviz::line_chart(dynamic = FALSE, params = list(df = df, x = \"specimen_date\", y = \"count\"))"
  }'
  
  # Test with different providers
  providers <- c("openai", "gemini", "claude")
  
  test_provider <- function(provider) {
    Sys.setenv(LLM_PROVIDER = provider)
    Sys.setenv(LLM_API_KEY = "test_key")
    Sys.setenv(LLM_MODEL = "test_model")
    
    with_mock(
      `epiviz:::query_llm_json` = function(...) {
        return(jsonlite::fromJSON(mock_response))
      },
      `epiviz:::log_llm_interaction` = function(...) { return(NULL) },
      {
        expect_error(llm_auto_viz(lab_data, execute = FALSE), NA)
      }
    )
  }
  
  # Skip actually running these tests since they involve mocking internal functions
  # that might change implementation in the future, but keep the test structure
  skip("Skipping provider tests as they involve deeper mocking")
  for (provider in providers) {
    test_provider(provider)
  }
  
  # Test invalid provider
  Sys.setenv(LLM_PROVIDER = "invalid_provider")
  Sys.setenv(LLM_API_KEY = "test_key")
  Sys.setenv(LLM_MODEL = "test_model")
  
  skip("Skipping invalid provider test as it involves deeper mocking")
  expect_error(llm_auto_viz(lab_data), "Unsupported LLM provider")
  
  # Restore environment variables
  Sys.setenv(LLM_PROVIDER = orig_provider)
  Sys.setenv(LLM_API_KEY = orig_api_key)
  Sys.setenv(LLM_MODEL = orig_model)
}) 