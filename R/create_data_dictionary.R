#' Create Data Dictionary
#'
#' This function takes a data frame and creates a data dictionary. The data
#' dictionary includes the variable name, a human-readable name, the variable
#' type, and a description. If a model is specified, the function uses OpenAI's
#' API to generate the information based on the characteristics of the data
#' frame.
#'
#' @param data A data frame to create a data dictionary for.
#' @param file_path The file path to save the data dictionary to.
#' @param model The ID of the OpenAI chat completion models to use for
#' generating descriptions (see `openai::list_models()`). If NULL (default), a
#' scaffolding for the data dictionary is created.
#' @param sample_n The number of rows to sample from the data frame to use as
#' input for the model. Default NULL.
#' @param grouping A character vector of column names to group by when sampling
#' rows from the data frame for the model. Default NULL.
#' @param force If TRUE, overwrite the file at `file_path` if it already exists.
#' Default FALSE.
#'
#' @return A data frame containing the variable name, human-readable name,
#' variable type, and description for each variable in the input data frame.
#'
#' @importFrom openai list_models create_chat_completion
#' @import dplyr
#'
#' @export
create_data_dictionary <-
  function(data,
           file_path,
           model = NULL,
           sample_n = 5,
           grouping = NULL,
           force = FALSE) {
    # Check to see if `data` is a data frame
    if (!is.data.frame(data)) {
      stop("`data` must be a data frame.")
    }
    # Check to see if `file_path` is a character string
    if (!is.character(file_path)) {
      stop("`file_path` must be a character string.")
    }
    # Check to see if file exists at `file_path` and `force` is FALSE
    if (file.exists(file_path) && !force) {
      # end function early
      return(message("File already exists at `file_path`.
                      Use `force = TRUE` to overwrite."))
    }
    # Check to see if `model` is NULL. If so, create dictionary scaffolding.
    # If not, check to see if `model` is one of the available models
    if (is.null(model)) {
      data_dict <-
        data.frame(
          variable = names(data),
          name = NA_character_,
          type = sapply(data, class, USE.NAMES = FALSE),
          description = NA_character_,
          stringsAsFactors = FALSE
        )
    } else {
      # Check to see if `model` is one of the available models
      if (!model %in% openai::list_models()[[2]]$id) {
        stop("`model` must be one of the available chat completion models.
             See `openai::list_models()`.")
      }
      # Check to see if OPENAI_API_KEY is set
      if (is.na(Sys.getenv()["OPENAI_API_KEY"])) {
        stop("Set your OPENAI_API_KEY environment variable.")
      }
      # Set the instructions for the prompt
      prompt_instructions <- "
      Create a data dictionary in raw CSV format for this dataset.
      Return ONLY the CSV data with these columns:
      - variable: exact column name from data
      - name: human readable name
      - type: one of 'categorical', 'ordinal' or 'numeric'
      - description: clear description of the variable and its values
      Important:
      - Enclose text values, e.g. values for 'description', in quotes to
        avoid misparsing of commas
      - Return ONLY the CSV data with no explanations or code blocks
        before or after the CSV data
      - Use variable names to infer info for null columns
      "
      # Get a the first 5 rows of the data frame
      data_sample <-
        data |>
        dplyr::slice_sample(
          n = sample_n,
          by = dplyr::all_of(grouping)
        ) |>
        # truncate character variables to 50 characters
        dplyr::mutate_if(
          is.character,
          function(x) substr(x, 1, 47) |> paste0("...")
        )
      # Convert the data sample to R code as a string
      prompt_data <-
        data_sample |>
        dput() |> # convert to R code
        utils::capture.output() |> # capture the output
        paste(collapse = " ") # collapse the output into a single string
      # Combine the instructions and the data sample into a single string
      prompt <- sprintf("%s\n\n%s", prompt_instructions, prompt_data)
      # Use openai to generate the descriptions for each of the variables in
      # the `data_sample` data frame.
      response <-
        openai::create_chat_completion(
          model = model,
          messages = list(list("role" = "user", "content" = prompt)),
          max_tokens = 500
        )
      # Create a data frame with the variable names, human-readable names,
      # and descriptions
      result <- tryCatch(
        {
          parsed_dict <- response$choices["message.content"] |> # get the response from the API
            as.character() |> # convert to a character vector
            textConnection() |> # create text connection
            utils::read.csv(stringsAsFactors = FALSE) |> # read the data dictionary as a data frame
            suppressMessages() # suppress messages
          list(success = TRUE, data = parsed_dict)
        },
        error = function(e) {
          warning("Failed to parse API response: ", e$message)
          list(success = FALSE, error = e$message)
        }
      )
      
      # If parsing failed, create a basic dictionary
      data_dict <- if (result$success) {
        result$data
      } else {
        warning("Falling back to basic dictionary structure")
        data.frame(
          variable = names(data),
          name = NA_character_,
          type = sapply(data, class, USE.NAMES = FALSE),
          description = NA_character_,
          stringsAsFactors = FALSE
        )
      }
      )
    }
    # Write the data dictionary to a file
    write.csv(data_dict, file = file_path, row.names = FALSE)
    # Return the data dictionary
    return(data_dict)
  }
