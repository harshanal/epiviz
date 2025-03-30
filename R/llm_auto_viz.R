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
llm_auto_viz <- function(df, user_prompt = "", dynamic = FALSE, execute = TRUE) {
  lifecycle::signal_stage("experimental", "llm_auto_viz()")
  
  # Check if required environment variables are set
  check_env_vars()
  
  # Get documentation for epiviz visualization functions
  epiviz_functions <- get_epiviz_function_info()
  
  # Extract data frame metadata (column names and types)
  df_metadata <- get_dataframe_metadata(df)
  
  # Build the system prompt for the LLM
  system_prompt <- build_llm_prompt(epiviz_functions, df_metadata, user_prompt, dynamic)
  
  # Query the LLM with the system prompt
  llm_response <- query_llm_json(system_prompt, df_metadata$column_names, user_prompt)
  
  # Extract information from the LLM response
  selected_function <- llm_response$selected_function
  r_code <- llm_response$r_code
  
  # Log the interaction for auditing
  log_llm_interaction(system_prompt, df_metadata, user_prompt, llm_response)
  
  # Execute the code or return it
  if (execute) {
    tryCatch({
      # Create a new environment to execute the code
      env <- new.env()
      # Make sure the data frame is available in the new environment
      env$df <- df
      # Evaluate the code in the new environment
      viz <- eval(parse(text = r_code), envir = env)
      return(viz)
    }, error = function(e) {
      message("Error executing the generated code: ", e$message)
      message("Generated code:\n", r_code)
      return(NULL)
    })
  } else {
    return(r_code)
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
#' @param epiviz_functions List of epiviz function documentation
#' @param df_metadata Data frame metadata
#' @param user_prompt User guidance (if any)
#' @param dynamic Whether to generate interactive visualization
#'
#' @return A structured system prompt for the LLM
#' @keywords internal
build_llm_prompt <- function(epiviz_functions, df_metadata, user_prompt, dynamic) {
  prompt <- paste0(
    "You are an expert R data visualization assistant specialized in the epiviz package. ",
    "Your task is to analyze the structure of a data frame and generate R code for the most appropriate visualization.\n\n",
    
    "Data Frame Metadata:\n",
    "- Columns: ", paste(df_metadata$column_names, " (", df_metadata$column_types, ")", sep="", collapse=", "), "\n",
    "- Rows: ", df_metadata$row_count, "\n\n",
    
    "Column Details:\n"
  )
  
  # Add detailed information about each column
  for (summary in df_metadata$column_summary) {
    prompt <- paste0(prompt, "- ", summary$name, " (", summary$type, "):\n")
    
    if (is.numeric(summary$unique_count)) {
      prompt <- paste0(prompt, "  * Unique values: ", summary$unique_count, "\n")
    }
    
    if (!is.null(summary$range)) {
      prompt <- paste0(prompt, "  * Range: [", summary$range[1], " to ", summary$range[2], "]\n")
    }
    
    if (!is.null(summary$date_range)) {
      prompt <- paste0(prompt, "  * Date range: [", summary$date_range[1], " to ", summary$date_range[2], "]\n")
      prompt <- paste0(prompt, "  * Span in days: ", summary$span_days, "\n")
    }
    
    if (!is.null(summary$is_high_cardinality)) {
      prompt <- paste0(prompt, "  * High cardinality: ", summary$is_high_cardinality, "\n")
    }
  }
  
  prompt <- paste0(
    prompt, "\n",
    "Available epiviz visualization functions:\n"
  )
  
  # Add documentation for each epiviz function
  for (func_name in names(epiviz_functions)) {
    func_info <- epiviz_functions[[func_name]]
    prompt <- paste0(
      prompt, 
      "## ", func_name, "\n",
      func_info$description, "\n",
      "Parameters:\n"
    )
    
    for (param_name in names(func_info$params)) {
      prompt <- paste0(
        prompt,
        "- ", param_name, ": ", func_info$params[[param_name]], "\n"
      )
    }
    prompt <- paste0(prompt, "\n")
  }
  
  # Add user guidance if provided
  if (nzchar(user_prompt)) {
    prompt <- paste0(
      prompt,
      "User guidance: ", user_prompt, "\n",
      "IMPORTANT: If the user guidance explicitly requests a specific visualization type, prioritize that request even if another visualization might be more appropriate based on the data structure.\n\n"
    )
  }
  
  # Add instructions for dynamic/static visualization
  prompt <- paste0(
    prompt,
    "Create a ", if(dynamic) "dynamic (interactive)" else "static", " visualization.\n\n"
  )
  
  # Add response format instructions
  prompt <- paste0(
    prompt,
    "Please respond ONLY with a JSON object in the following format, with no additional text or explanation:\n",
    "{\n",
    "  \"selected_function\": \"<chosen epiviz function name>\",\n",
    "  \"r_code\": \"<fully executable R code snippet using the chosen epiviz function>\"\n",
    "}\n\n",
    "The R code should be properly formatted, executable without errors, and should use the data frame 'df' directly without modification unless necessary for the visualization. Include comments explaining key aspects of the code."
  )
  
  return(prompt)
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
#'
#' @param system_prompt The system prompt sent to the LLM
#' @param df_metadata Metadata about the data frame (no actual data)
#' @param user_prompt The user's guidance (if any)
#' @param llm_response The response from the LLM
#'
#' @keywords internal
log_llm_interaction <- function(system_prompt, df_metadata, user_prompt, llm_response) {
  log_path <- file.path(getwd(), "llm_audit_log.json")
  
  log_entry <- list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    column_names = df_metadata$column_names,
    column_types = df_metadata$column_types,
    row_count = df_metadata$row_count,
    user_prompt = user_prompt,
    selected_function = llm_response$selected_function,
    provider = Sys.getenv("LLM_PROVIDER"),
    model = Sys.getenv("LLM_MODEL")
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