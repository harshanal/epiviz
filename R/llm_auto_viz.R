#' Automated Visualization Generation Using LLM
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function automatically selects and generates R visualization code
#' based on the provided data frame structure using a Large Language Model.
#' Only metadata (column names and types) is sent to the LLM, maintaining data privacy.
#'
#' @param df A data frame to visualise
#' @param user_prompt Character string providing optional guidance for visualization selection
#' @param dynamic Logical; whether to generate an interactive (TRUE) or static (FALSE) visualization
#' @param execute Logical; whether to execute the generated code (TRUE) or just return it (FALSE)
#'
#' @return If execute=TRUE, returns the visualization. If execute=FALSE, returns the R code as a character string.
#'
#' @details
#' This function examines the structure of the provided data frame and uses an LLM to generate
#' appropriate visualization code using epiviz functions. It prioritizes user guidance when provided.
#' All LLM interactions are logged for auditing purposes.
#'
#' **Environment Variables:**
#' - `LLM_PROVIDER`: Specifies the LLM provider ("openai", "gemini", "claude").
#' - `LLM_API_KEY`: The API key corresponding to the chosen provider.
#' - `LLM_MODEL`: The model identifier to use.
#'
#' @examples
#' \dontrun{
#' # Automatic visualization
#' viz <- llm_auto_viz(lab_data())
#'
#' # Specify a visualization type
#' viz <- llm_auto_viz(lab_data(), user_prompt = "Use a line chart to show weekly trends.")
#'
#' # Get code without executing it
#' code <- llm_auto_viz(lab_data(), execute = FALSE)
#' cat(code)
#' }
#'
#' @import ellmer
#' @importFrom lifecycle badge signal_stage
#' @importFrom jsonlite fromJSON
#' @export
llm_auto_viz <- function(df, user_prompt = "",  execute = TRUE) {
  lifecycle::signal_stage("experimental", "llm_auto_viz()")

  # Capture the name of the data frame argument provided by the user
  df_name <- deparse(substitute(df))

  # Check if required environment variables are set
  check_env_vars()

  # Get documentation for epiviz visualization functions
  epiviz_functions <- get_epiviz_function_info()

  # Extract data frame metadata (column names and types)
  # Pass the actual data frame object here
  df_metadata <- get_dataframe_metadata(df)

  # Build the system prompt for the LLM
  system_prompt <- build_llm_prompt(df_metadata, user_prompt)

  # Query the LLM with the system prompt
  llm_response <- query_llm_json(system_prompt, df_metadata$column_names, user_prompt)

  # Extract information from the LLM response
  selected_function <- llm_response$selected_function
  r_code <- llm_response$r_code

  # Log the interaction for auditing
  log_llm_interaction(system_prompt, df_metadata, user_prompt, llm_response)

  # --- Substitute the placeholder dataframe name with the actual name --- #
  # Check if r_code is not NULL or an error message
  if (!is.null(r_code) && !startsWith(r_code, "#")) {
    # Simple pattern to find df = df or data = df in the params list
    r_code <- gsub("(df|data)\\s*=\\s*df", paste0("df = ", df_name), r_code)
  }
  # --------------------------------------------------------------------------- #

  # Execute the code or return it
  if (execute) {
    tryCatch({
      # --- Execute using the original data frame from the calling environment --- #
      # We need the actual data frame object here, which is 'df'.
      # The modified r_code string now correctly references df_name,
      # which should exist in the environment where llm_auto_viz was called.
      viz <- eval(parse(text = r_code), envir = parent.frame()) # Evaluate in calling environment
      return(viz)
    }, error = function(e) {
      message("Error executing the generated code: ", e$message)
      # Print the *modified* code that caused the error
      message("Generated code (after df name substitution):\n", r_code)
      return(NULL)
    })
  } else {
    # Use cat() to print the raw code for easy copy-pasting
    cat(r_code)
    # Return the code invisibly to avoid R printing it again with [1]
    invisible(r_code)
  }
}

#' Check Required Environment Variables
#'
#' @keywords internal
check_env_vars <- function() {
  # Check for required environment variables with informative messages
  tryCatch({
    provider <- Sys.getenv("LLM_PROVIDER")
    if (provider == "") {
      stop("LLM_PROVIDER environment variable is not set. ",
           "Please set it to one of: 'openai', 'gemini', or 'claude'")
    }

    api_key <- Sys.getenv("LLM_API_KEY")
    if (api_key == "") {
      stop("LLM_API_KEY environment variable is not set. ",
           sprintf("For %s, please set the appropriate API key.",
                   toupper(provider)))
    }

    model <- Sys.getenv("LLM_MODEL")
    if (model == "") {
      stop("LLM_MODEL environment variable is not set. ",
           sprintf("For %s, please set an appropriate model identifier.",
                   toupper(provider)))
    }
  }, error = function(e) {
    stop("Error checking environment variables: ", conditionMessage(e))
  })
}

#' Get Data Frame Metadata
#'
#' Extracts column names and types from a data frame without including actual data.
#'
#' @param df A data frame
#' @return A list containing column names and types
#' @keywords internal
get_dataframe_metadata <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  column_names <- names(df)

  # Get column types in a privacy-preserving way
  column_types <- sapply(df, function(col) {
    if (is.factor(col)) return("factor")
    if (is.character(col)) return("character")
    if (is.numeric(col)) return("numeric")
    if (is.integer(col)) return("integer")
    if (inherits(col, "Date")) return("Date")
    if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) return("datetime")
    if (is.logical(col)) return("logical")
    return("other")
  })

  # Get column summary in a privacy-preserving way
  column_summary <- lapply(names(df), function(col_name) {
    col <- df[[col_name]]
    summary <- list(
      name = col_name,
      type = class(col)[1],
      unique_count = length(unique(col))
    )

    # Add type-specific information without revealing data
    if (is.numeric(col) || is.integer(col)) {
      summary$range <- c(min(col, na.rm = TRUE), max(col, na.rm = TRUE))
      summary$has_negative <- any(col < 0, na.rm = TRUE)
      summary$has_zero <- any(col == 0, na.rm = TRUE)
    }
    if (is.factor(col) || is.character(col)) {
      # Count number of unique values but don't include actual values
      summary$unique_count <- length(unique(col))
      summary$is_high_cardinality <- summary$unique_count > 20
    }
    if (inherits(col, "Date") || inherits(col, "POSIXct") || inherits(col, "POSIXlt")) {
      summary$date_range <- c(min(col, na.rm = TRUE), max(col, na.rm = TRUE))
      summary$span_days <- as.numeric(diff(range(col, na.rm = TRUE)))
    }

    return(summary)
  })

  return(list(
    column_names = column_names,
    column_types = column_types,
    column_summary = column_summary,
    row_count = nrow(df),
    column_count = ncol(df),
    has_na = any(is.na(df))
  ))
}

#' Get epiviz Function Documentation
#'
#' Dynamically extracts documentation for epiviz visualization functions.
#'
#' @return A list containing function documentation
#' @keywords internal
get_epiviz_function_info <- function() {
  viz_functions <- c("line_chart", "point_chart", "col_chart", "epi_curve", "age_sex_pyramid")
  function_info <- list()

  for (func_name in viz_functions) {
    # Check if the function exists
    if (!exists(func_name, mode = "function")) {
      next
    }

    # Get function documentation
    func_docs <- tryCatch({
      utils::help(func_name, package = "epiviz")
      # Create temporary file to capture the help output
      temp_file <- tempfile()
      utils::capture.output(
        print(utils::help(func_name, package = "epiviz")),
        file = temp_file
      )
      docs <- readLines(temp_file)
      unlink(temp_file)

      # Process documentation
      title <- ""
      description <- ""
      params <- list()

      in_params <- FALSE

      for (line in docs) {
        if (grepl("^Description:", line)) {
          in_description <- TRUE
          in_params <- FALSE
          next
        } else if (grepl("^Arguments:", line)) {
          in_description <- FALSE
          in_params <- TRUE
          next
        } else if (grepl("^Value:", line) || grepl("^Details:", line) || grepl("^Examples:", line)) {
          in_description <- FALSE
          in_params <- FALSE
        }

        if (grepl("^Title:", line)) {
          title <- trimws(gsub("^Title:", "", line))
        } else if (in_description) {
          description <- paste(description, trimws(line))
        } else if (in_params && nchar(trimws(line)) > 0) {
          param_match <- regmatches(line, regexec("^\\s+([A-Za-z0-9_]+):\\s+(.*)", line))
          if (length(param_match[[1]]) >= 3) {
            param_name <- param_match[[1]][2]
            param_desc <- param_match[[1]][3]
            params[[param_name]] <- param_desc
          }
        }
      }

      list(
        title = title,
        description = trimws(description),
        params = params
      )
    }, error = function(e) {
      # In case of error, provide a minimal description based on function inspection
      func <- get(func_name)
      params <- names(formals(func))
      param_list <- setNames(rep("", length(params)), params)

      list(
        title = paste(func_name, "visualization"),
        description = paste("Creates a", func_name, "visualization."),
        params = param_list
      )
    })

    function_info[[func_name]] <- func_docs
  }

  return(function_info)
}

#' Build LLM System Prompt
#'
#' Constructs a comprehensive system prompt for the LLM based on epiviz function
#' documentation and data frame metadata.
#'
#' @param df_metadata Data frame metadata
#' @param user_prompt User guidance (if any)
#'
#' @return A structured system prompt for the LLM
#' @keywords internal
build_llm_prompt <- function(df_metadata, user_prompt) {

  # Define the static string containing epiviz function documentation
  static_func_docs <- "
Available epiviz visualization functions:

## line_chart
- Description: Creates a line chart showing trends over time. Best used to show how numeric values change over a time period.
- Parameters:
    - df: Data frame to visualize (REQUIRED in params list)
    - x: Column name for x-axis (REQUIRED). MUST be a date/datetime column from the data frame. This represents your time axis.
    - y: Column name for y-axis (REQUIRED). MUST be a numeric column from the data frame. This represents the value being measured over time.
    - group_var: Optional: column for grouping lines. If provided, must be a categorical column (character/factor) with a reasonable number of unique values (ideally < 10).
    - line_colour: Optional: vector of colours by group
    - line_type: Optional: vector of line types (e.g. 'solid', 'dashed')
    - dual_axis: Optional: logical to include secondary y-axis
    - threshold_lines: Optional: numeric vector for reference lines
    - ci: Optional: logical to include confidence intervals
- Validation Rules:
    1. 'x' MUST be a column of type 'Date' or 'datetime' from the data frame
    2. 'y' MUST be a column of type 'numeric' or 'integer' from the data frame
    3. 'group_var' if provided, MUST be a categorical column (character/factor)
    4. Empty strings ('') are NOT allowed for any column name
- Column Selection Guidelines:
    1. For 'x': Choose a date/time column that represents when measurements were taken. If multiple are available, prefer the one with the widest range.
    2. For 'y': Choose a numeric column that represents a quantity being measured. If multiple are available, prefer the one with the widest range or most variability.
    3. For 'group_var': Choose a categorical column that meaningfully splits the data into groups. Prefer columns with a moderate number of unique values.
    4. NEVER use:
       - Categorical columns (character/factor) for 'y'
       - ID columns for 'y' or 'group_var'
       - Numeric columns that are actually codes/IDs
       - Empty strings for any column name
- If no valid numeric column is available for 'y', return a message indicating this limitation.

## col_chart
- Description: Column or bar chart for categorical comparisons.
- Parameters:
    - df: Data frame to visualize (REQUIRED in params list)
    - x: Column name for x-axis (REQUIRED, categorical). Must be an exact column name from the data frame.
    - y: Column name for y-axis (REQUIRED, numeric). Must be an exact column name from the data frame and contain numeric values.
    - group_var: Optional: column for grouping bars. If provided, must be an exact column name from the data frame.
    - dual_axis: Optional: logical
    - threshold_lines: Optional: reference lines for y-axis
    - ci: Optional: include confidence intervals
- Validation Rules:
    1. 'x' must exist in the data frame columns
    2. 'y' must exist in the data frame columns and be numeric
    3. if 'group_var' is provided, it must exist in the data frame columns

## point_chart
- Description: Scatter plot for exploring relationships between variables.
- Parameters:
    - df: Data frame (REQUIRED in params list)
    - x: X-axis (REQUIRED, numeric or categorical). Must be an exact column name from the data frame.
    - y: Y-axis (REQUIRED, numeric). Must be an exact column name from the data frame and contain numeric values.
    - group_var: Optional: column to group points. If provided, must be an exact column name from the data frame.
    - size_var: Optional: column to vary point size. If provided, must be numeric column from the data frame.
    - shape_var: Optional: column to vary point shape. If provided, must be an exact column name from the data frame.
    - ci: Optional: logical for confidence intervals
- Validation Rules:
    1. 'x' must exist in the data frame columns
    2. 'y' must exist in the data frame columns and be numeric
    3. if any optional variables are provided, they must exist in the data frame columns

## epi_curve
- Description: Epidemic curve showing case distribution over time.
- Parameters:
    - df: Data frame to visualize (REQUIRED in params list)
    - date_col: Date column for time axis (REQUIRED). Must be an exact column name from the data frame containing dates.
    - case_col: Column with case counts (REQUIRED). Must be an exact column name from the data frame containing numeric values.
    - group_var: Optional: grouping variable for curves. If provided, must be an exact column name from the data frame.
    - time_period: Optional: Aggregation period: 'day', 'week', or 'month'
    - rolling_average: Optional: smoothing window (integer)
    - cumulative: Optional: logical to show cumulative cases
- Validation Rules:
    1. 'date_col' must exist in the data frame columns and be a date/datetime type
    2. 'case_col' must exist in the data frame columns and be numeric
    3. if 'group_var' is provided, it must exist in the data frame columns

## age_sex_pyramid
- Description: Age-sex demographic pyramid for population or cases.
- Parameters:
    - df: Data frame to visualize (REQUIRED in params list)
    - var_map: Named list mapping 'date_of_birth' and 'sex' column names (REQUIRED). Both referenced columns must exist in the data frame.
    - grouped: Optional: logical for grouped pyramids
    - colours: Optional: vector of colours for each sex
    - ci: Optional: confidence interval display
- Validation Rules:
    1. Both columns referenced in var_map must exist in the data frame
    2. The date_of_birth column must be a date type
    3. The sex column must be a character or factor type

IMPORTANT COLUMN SELECTION RULES:
1. ALL column names used in parameters MUST be exact matches from the available columns in the data frame.
2. For numeric values (y, case_col, etc.), select columns of type 'numeric' or 'integer'.
3. For dates (x in line_chart, date_col in epi_curve), select columns of type 'Date' or 'datetime'.
4. For categorical grouping (group_var), prefer columns of type 'factor' or 'character' with reasonable cardinality.
5. Double-check that selected columns exist in the data frame metadata provided above.
"

  # Core instructions
  prompt_parts <- c()

  prompt_parts <- c(prompt_parts, paste0(
    "You are an expert R visualization assistant using the epiviz package.\n",
    "Your task is to analyze the structure of the data frame and generate executable R code using one of the epiviz visualization functions.\n\n",
    "PROCESS STEPS:\n",
    "1. Analyze the Data Frame Metadata and Column Details provided below.\n",
    "2. Based on the metadata and any User Guidance, select the most appropriate epiviz visualization function from the 'Available epiviz visualization functions' list.\n",
    "3. Identify the REQUIRED parameters for the selected function.\n",
    "4. For each REQUIRED parameter (like 'x', 'y', 'date_col', 'case_col'), select a SPECIFIC, EXACT column name from the 'Data Frame Metadata' that matches the required type (e.g., 'Date'/'datetime' for 'x' in line_chart, 'numeric'/'integer' for 'y').\n",
    "5. CRITICAL FOR 'y' PARAMETER (and similar numeric parameters): You MUST choose ONE existing numeric/integer column name. If multiple suitable numeric columns exist, choose one based on the 'Column Selection Guidelines'. If NO suitable numeric column exists, set 'selected_function' to NULL and 'r_code' to '# No suitable numeric column found for the required parameter.' DO NOT use placeholders (e.g., '<numeric_column_here>') or empty strings ('').\n",
    "6. Select optional parameters (like 'group_var') if appropriate, ensuring they also use EXACT column names and meet type requirements.\n",
    "7. Construct the R code string following the 'IMPORTANT INSTRUCTIONS FOR R CODE GENERATION' below.\n",
    "8. Format the final output as a minified JSON object as specified in 'RESPONSE FORMAT'.\n\n"
  ))

  # Add data frame metadata
  prompt_parts <- c(prompt_parts, paste0(
    "Data Frame Metadata:\n",
    "- Columns: ", paste(df_metadata$column_names, " (", df_metadata$column_types, ")", sep="", collapse=", "), "\n",
    "- Rows: ", df_metadata$row_count, "\n\n",
    "Column Details:\n"
  ))

  # Add detailed information about each column
  col_details <- character(0)
  for (summary in df_metadata$column_summary) {
    col_info <- paste0("- ", summary$name, " (", summary$type, "):\n")

    if (is.numeric(summary$unique_count)) {
      col_info <- paste0(col_info, "  * Unique values: ", summary$unique_count, "\n")
    }

    if (!is.null(summary$range)) {
      col_info <- paste0(col_info, "  * Range: [", summary$range[1], " to ", summary$range[2], "]\n")
    }

    if (!is.null(summary$date_range)) {
      col_info <- paste0(col_info, "  * Date range: [", summary$date_range[1], " to ", summary$date_range[2], "]\n")
      col_info <- paste0(col_info, "  * Span in days: ", summary$span_days, "\n")
    }

    if (!is.null(summary$is_high_cardinality)) {
      col_info <- paste0(col_info, "  * High cardinality: ", summary$is_high_cardinality, "\n")
    }
    
    col_details <- c(col_details, col_info)
  }
  prompt_parts <- c(prompt_parts, paste(col_details, collapse=""))

  # Add the static function documentation string
  prompt_parts <- c(prompt_parts, static_func_docs)

  # Add user guidance if provided
  if (nzchar(user_prompt)) {
    prompt_parts <- c(prompt_parts, paste0(
      "User guidance: ", user_prompt, "\n",
      "IMPORTANT: If the user guidance explicitly requests a specific visualization type, prioritize that request.\n\n"
    ))
  }

  # Add response format and R code instructions
  prompt_parts <- c(prompt_parts, paste0(
    "RESPONSE FORMAT:\n",
    "You MUST respond with ONLY a raw, minified JSON object. DO NOT include any markdown formatting (no ```json or ``` blocks).\n",
    "The response must be exactly in this format: {\"selected_function\":\"<function_name_or_null>\",\"r_code\":\"<escaped_R_code_or_message>\"}\n",
    "The 'r_code' value must be a single-line R code string with all line breaks escaped as \\n and all quotes properly escaped.\n",
    "**Set the `dynamic` parameter based on these rules:**\n",
    "1. If the user prompt contains words like 'dynamic', 'interactive', or 'plotly', set `dynamic = TRUE`\n",
    "2. Otherwise, set `dynamic = FALSE` (default)\n\n",

    "IMPORTANT INSTRUCTIONS FOR R CODE GENERATION:\n",
    "1. Follow the 'PROCESS STEPS' above carefully.\n",
    "2. CRITICAL REITERATION: ALL column names used in parameters MUST be EXACT matches from the 'Data Frame Metadata' columns. NO PLACEHOLDERS. NO EMPTY STRINGS. Use the real column names provided.\n",
    "3. If generating code (not an error message):
",
    "   a. The R code MUST call the selected epiviz function (e.g., `epiviz::line_chart`).\n",
    "   b. The FIRST argument MUST be `dynamic = TRUE/FALSE` based on the rules above.\n",
    "   c. The SECOND argument MUST be `params = list(...)`.\n",
    "   d. Inside the `params` list, the FIRST item MUST be the data frame, written exactly as `df = df`.\n",
    "   e. Include ALL REQUIRED parameters for the chosen function within the `params` list, using the EXACT column names selected in the process steps.\n",
    "   f. Verify parameter types against the function's 'Validation Rules'.\n",
    "4. If NO suitable numeric column is found for a required numeric parameter (like 'y'), the 'r_code' MUST be exactly: '# No suitable numeric column found for the required parameter.' and 'selected_function' should be null.\n",
    "5. Escape the final R code string for JSON (line breaks as \\n, quotes as \").\n\n",
    "DO NOT include any text before or after the JSON object. The response must start with '{' and end with '}'."
  ))

  # Combine all parts into a single string
  return(paste(prompt_parts, collapse=""))
}

#' Query LLM API for JSON Response
#'
#' Sends a prompt to the LLM API and retrieves a structured JSON response using the ellmer package.
#'
#' @param system_prompt System prompt for the LLM
#' @param column_names Column names for logging
#' @param user_prompt User guidance for logging
#'
#' @return A parsed JSON object containing the selected function and R code
#' @keywords internal
query_llm_json <- function(system_prompt, column_names, user_prompt) {
  provider <- Sys.getenv("LLM_PROVIDER")
  api_key <- Sys.getenv("LLM_API_KEY")
  model <- Sys.getenv("LLM_MODEL")

  tryCatch({
    # Initialize the chat client
    chat <- switch(
      provider,
      "openai" = ellmer::chat_openai(model = model, api_key = api_key),
      "gemini" = ellmer::chat_gemini(model = model, api_key = api_key),
      "claude" = ellmer::chat_claude(model = model, api_key = api_key),
      stop(sprintf("Unsupported LLM provider: '%s'", provider))
    )

    # Ensure the system prompt is a single string
    if (length(system_prompt) > 1) {
      system_prompt <- paste(system_prompt, collapse = "")
    }

    # Set the system prompt
    chat$set_system_prompt(system_prompt)

    # Send the prompt to the API
    response <- chat$chat("Generate the visualization code based on the data structure.")

    # Parse JSON response
    json_response <- jsonlite::fromJSON(response)

    # Validate the response structure
    if (!all(c("selected_function", "r_code") %in% names(json_response))) {
      stop("Invalid LLM response structure. Missing required fields.")
    }

    # Unescape the R code string
    json_response$r_code <- gsub('\\\\"', '"', json_response$r_code)

    return(json_response)

  }, error = function(e) {
    # Log the error
    log_path <- file.path(getwd(), "llm_error_log.json")
    error_log <- list(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      error = e$message,
      column_names = column_names,
      user_prompt = user_prompt
    )

    tryCatch({
      # Append to existing log if it exists
      if (file.exists(log_path)) {
        logs <- jsonlite::fromJSON(log_path)
        logs <- c(logs, list(error_log))
        jsonlite::write_json(logs, log_path, auto_unbox = TRUE, pretty = TRUE)
      } else {
        jsonlite::write_json(list(error_log), log_path, auto_unbox = TRUE, pretty = TRUE)
      }
    }, error = function(e) {
      message("Failed to log error: ", e$message)
    })

    stop("Error querying LLM API: ", e$message)
  })
}

#' Log LLM Interaction
#'
#' Records details of LLM interactions for auditing purposes.
#' Captures complete information about what was sent to the LLM API for transparency.
#'
#' @param system_prompt The system prompt sent to the LLM
#' @param df_metadata Metadata about the data frame (no actual data)
#' @param user_prompt The user's guidance (if any)
#' @param llm_response The response from the LLM
#'
#' @keywords internal
log_llm_interaction <- function(system_prompt, df_metadata, user_prompt, llm_response) {
  log_path <- file.path(getwd(), "llm_audit_log.json")

  # Process column summaries to ensure they're JSON-serializable
  processed_summaries <- lapply(df_metadata$column_summary, function(summary) {
    # Convert any special objects to strings
    if (!is.null(summary$date_range)) {
      summary$date_range <- as.character(summary$date_range)
    }
    if (!is.null(summary$range)) {
      summary$range <- as.numeric(summary$range)
    }
    summary
  })

  # Create a comprehensive audit entry
  log_entry <- list(
    # Timestamp and session info
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    session_info = list(
      provider = Sys.getenv("LLM_PROVIDER"),
      model = Sys.getenv("LLM_MODEL")
    ),
    
    # Input sent to LLM
    input = list(
      # The complete system prompt shows exactly what instructions were given to the LLM
      system_prompt = system_prompt,
      user_prompt = user_prompt %||% "",
      
      # Detailed data frame information shows what metadata was shared
      data_frame_info = list(
        column_names = as.character(df_metadata$column_names),
        column_types = as.character(df_metadata$column_types),
        column_summaries = processed_summaries,
        dimensions = list(
          rows = df_metadata$row_count,
          columns = df_metadata$column_count
        ),
        has_missing_values = df_metadata$has_na
      )
    ),
    
    # LLM Response
    output = list(
      selected_function = llm_response$selected_function,
      generated_code = llm_response$r_code
    )
  )

  tryCatch({
    # Append to existing log if it exists
    if (file.exists(log_path)) {
      logs <- jsonlite::fromJSON(log_path)
      # Check if logs is a list of entries or a single entry
      if (!is.null(names(logs)) && !is.list(logs[[1]])) {
        logs <- list(logs)
      }
      logs <- c(logs, list(log_entry))
      jsonlite::write_json(logs, log_path, auto_unbox = TRUE, pretty = TRUE)
    } else {
      jsonlite::write_json(list(log_entry), log_path, auto_unbox = TRUE, pretty = TRUE)
    }
  }, error = function(e) {
    message("Failed to log LLM interaction: ", e$message)
  })
}
