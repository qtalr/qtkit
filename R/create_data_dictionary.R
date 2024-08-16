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
#' @importFrom tibble tibble
#' @importFrom glue glue
#' @importFrom purrr map_chr
#' @importFrom stringr str_trunc
#' @importFrom openai list_models create_chat_completion
#' @import dplyr
#' @import readr
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
        tibble::tibble(
          variable = names(data),
          name = NA_character_,
          type = purrr::map_chr(data, class),
          description = NA_character_
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
      I have a dataset I would like you to create a data dictionary for.
      The information I want is the `variable`, `name` (human readable),
      `type` (one of 'categorical', 'ordinal' or 'numeric'), and `description`.
      Here's a small sample of the data for you to work with. In some cases
      some variables in the dataset may be null. In these cases use the
      variable name to predict the other information. Please return your data
      dictionary in raw CSV format. Remember to enclose text in quotes and only
      return the formatted data, no explanations.
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
          stringr::str_trunc,
          width = 50
        )

      # Convert the data sample to R code as a string
      prompt_data <-
        data_sample |>
        dput() |> # convert to R code
        utils::capture.output() |> # capture the output
        paste(collapse = " ") # collapse the output into a single string

      # Combine the instructions and the data sample into a single string
      prompt <- glue::glue("{prompt_instructions}\n\n{prompt_data}")

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
      data_dict <-
        response$choices["message.content"] |> # get the response from the API
        as.character() |> # convert to a character vector
        readr::read_csv() |> # read the data dictionary as a data frame
        suppressMessages() # suppress messages
    }
    # Write the data dictionary to a file
    data_dict |> readr::write_csv(file = file_path)

    # Return the data dictionary
    return(data_dict)
  }
