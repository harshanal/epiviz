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

