
#' organize the output of a solo object into a table.
#' @param solo_input an object returned by the analyze function
#' @param type one of "long", "wide" or c("long","wide").
#' @param filler_value the value that goes into blank cells
#' @importFrom dplyr 'case_when' 'bind_rows' 'mutate_at'
#' @importFrom purrr 'map_chr' 'map_dfr' 'map_int'
#' @importFrom tidyr 'unnest' 'spread'
#' @importFrom forcats 'fct_recode'
#' @importFrom stringr 'str_split'
#' @importFrom tibble 'tibble'
#' @importFrom knitr 'kable'
#' @importFrom kableExtra 'add_indent'
#' @export


codify <- function(
  solo_input,
  type = c('long','wide'),
  filler_value = ' '
){

  outcome = exposure = . = formula = term = variable =  NULL
  desc_column = fits_column = estm_column = NULL
  ests = fits = tbl = table_row = stat = NULL
  table_value = name = new_name = group = NULL

  analysis <- solo_input$analysis
  solabels <- solo_input$labels
  analysis_type <- solo_input$type

  label_key <-
    pluck(solabels$variables$label) %>%
    set_names(solabels$variables$name)

  units_key <-
    solabels$variables %>%
    dplyr::mutate(
      new_label = case_when(
        !is.na(unit) ~ paste0(label,', per ', unit),
        TRUE ~ label
      )
    ) %>%
    purrr::pluck('new_label') %>%
    set_names(solabels$variables$label)


  # Determine values for column variables -----------------------------------

  column_classes <- map_chr(
    analysis,
    ~ {
      if(class(.x)=='list') class(.x[[1]])[1] else {"nada"}
    }
  )

  if(is.null(desc_column)){
    desc_indx <-
      grep(x = column_classes, pattern = "descriptives", fixed = TRUE)
    if(length(desc_indx) > 1){
      stop(
        "Too many estimate columns in the input:", "\n",
        paste(column_classes[desc_indx]), "\n",
        "(There should only be one)"
      )
    }
    desc_column = names(column_classes)[desc_indx]
  }

  if(is.null(fits_column)){
    fits_indx <-
      grep(x = column_classes, pattern = "_fit", fixed = TRUE)
    if(length(fits_indx) > 1){
      stop(
        "Too many estimate columns in the input:", "\n",
        paste(column_classes[fits_indx]), "\n",
        "(There should only be one)"
      )
    }

    ratio_label = if(analysis_type=='rel_risk'){
      "Prevalence ratio (95% confidence interval)"
    } else if(analysis_type=='prop_haz'){
      "Hazard ratio (95% confidence interval)"
    } else {
      "Don't have a label for that"
    }

    fits_column = names(column_classes)[fits_indx]
  }

  if(is.null(estm_column)){
    estm_indx <-
      grep(x = column_classes, pattern = "model_estimates", fixed = TRUE)
    if(length(estm_indx) > 1){
      stop(
        "Too many estimate columns in the input:", "\n",
        paste(column_classes[estm_indx]), "\n",
        "(There should only be one)"
      )
    }
    estm_column = names(column_classes)[estm_indx]
  }

  output <- vector(
    mode = 'list',
    length = length(type)
  ) %>%
    set_names(type)

  # Long table --------------------------------------------------------------

  if('long' %in% type){

    long_tbl <- analysis %>%
      dplyr::group_by(outcome, exposure) %>%
      tidyr::nest() %>%
      base::split(f=.$outcome) %>%
      purrr::map(
        .f = function(block){

          main_estms = block$data[[1]]$main_estms %>%
            map(~.x$numeric) %>%
            set_names(block$data[[1]]$formula) %>%
            bind_rows(.id='formula')

          supp_estms = block$data[[1]]$supp_estms %>%
            map(
              .f = function(.control){
                map_dfr(.control, ~ .x$numeric)
              }
            ) %>%
            set_names(block$data[[1]]$formula) %>%
            dplyr::bind_rows(.id = 'formula') %>%
            mutate(
              variable = case_when(
                tolower (variable) == 'yes' ~ term,
                TRUE ~ variable
              )
            )

          tabdat <- rbind(main_estms, supp_estms) %>%
            dplyr::select(
              formula, term, variable, table_value
            )

          if (any(grepl(pattern = "._solo_.", x = tabdat$variable))) {

            tabdat %<>% tidyr::separate(
              col='variable', sep='._solo_.',
              into = c("variable", "group"),
              extra = 'drop', fill = 'right'
            ) %>%
              mutate_at(
                c('group'),
                ~case_when(
                  is.na(.x) ~ " ",
                  TRUE ~ .x
                )
              ) %>%
              dplyr::select(group,formula, variable, table_value)

          } else {

            tabdat %<>%
              dplyr::select(group = term, formula, variable, table_value)

          }

          tabdat %<>% spread(formula, table_value)

          # tabdat %<>% tidyr::pivot_wider(
          #   names_from = formula,
          #   values_from = table_value
          # )

          runs <- rle(tabdat$group)
          runs$values[runs$lengths==1] <- ' '

          group_indx <-
            runs$lengths %>%
            set_names(dplyr::recode(runs$values, !!!label_key))

          n_models <- length(block$data[[1]]$formula)

          header <-
            c(1, n_models) %>%
            set_names(c(" ", ratio_label))

          mdl_names <- setdiff(
            names(tabdat),
            c('group','variable', 'Unadjusted')
          )

          for(i in seq_along(mdl_names)){
            replace_indx <- which(names(tabdat)==mdl_names[i])
            names(tabdat)[replace_indx] %<>%
              paste0(
                kableExtra::footnote_marker_symbol(i), .
              )
          }

          tab = tabdat %>%
            dplyr::select(-group) %>%
            dplyr::mutate(
              variable = dplyr::recode(variable, !!!units_key)
            ) %>%
            dplyr::rename(Characteristic=variable) %>%
            knitr::kable(
              align = c("l",rep("c",ncol(.)-1)),
              escape = FALSE
            ) %>%
            kableExtra::kable_styling(
              bootstrap_options = c("striped","hover"),
              full_width = TRUE
            ) %>%
            kableExtra::add_header_above(header = header) %>%
            kableExtra::add_footnote(
              label = solabels$footnote,
              notation = 'symbol'
            )

          if(any(group_indx > 1)){
            tab %>% kableExtra::pack_rows(index = group_indx)
          } else {
            tab
          }

        }
      )

    output$long <- long_tbl

  }

  # Wide table --------------------------------------------------------------

  if('wide' %in% type){

    # Descriptive table rows
    desc_tab <- analysis %>%
      # pull descriptive column with identifies
      dplyr::select(outcome, exposure, !!desc_column) %>%
      # remove duplicates
      unique() %>%
      # turn data into a wide table
      spread(exposure, !!desc_column)

    # Remove the additional columns named table_row
    # (there are duplicates if more than one exposure is used)
    if(ncol(desc_tab)>=3){
      desc_tab %<>%
        mutate_at(
          3:ncol(.),
          ~ map(.x, function(..x){
            ..x[['table_row']]=NULL
            ..x
          })
        )
    }

    # After cleaning the inner tibbles, unnest
    desc_tab %<>% unnest(.sep = '._solo_.') %>%
      # designate these rows as 'descriptive'
      mutate(stat='desc')

    # table row name needs to be left as-is
    names(desc_tab)[2] <- "table_row"

    # Model estimate table rows
    ests_tab <- analysis %>%
      # rename columns so its easier to access them
      dplyr::rename(
        fits = !!fits_column,
        ests = !!estm_column
      ) %>%
      mutate(
        tbl = map2(
          ests, # table estimates live here
          fits, # p-values live here
          # bind each table estimate with corresponding p-values
          .f = ~ cbind(.x$tabular, .y$tests)
        )
      ) %>%
      dplyr::select(
        outcome, exposure, table_row = formula, tbl
      ) %>%
      # turn into a wide table
      spread(exposure, tbl) %>%
      unnest(.sep='._solo_.') %>%
      # designate these table rows as model estimates
      mutate(stat='ests')

    # Binding descriptives with model estimates
    # these names have to be added to the desc_tab so
    # that it can be stacked on top of the ests_tab
    names_to_add <- setdiff(
      names(ests_tab),
      names(desc_tab)
    )

    # make these values blank or whatever is specified
    for(nm in names_to_add){
      desc_tab[[nm]] <- filler_value
    }

    # re-order columns of desc_tab to match ests_tab
    desc_tab = desc_tab[,names(ests_tab)]

    # bind the two tables together by row
    tab <- rbind(desc_tab, ests_tab) %>%
      # maintain the proper table row order
       dplyr::mutate(table_row = fct_inorder(table_row)) %>%
      # group table by outcomes
      dplyr::arrange(outcome) %>%
      # place the table_row identifier as the first column
      # (this is because the first column is deleted later)
      dplyr::select(stat, everything())

    if(analysis_type=='prop_haz'){

      new_outcome_vals <- split_surv(levels(tab$outcome))
      new_outcome_labs <- rep("", length(new_outcome_vals))

      for(i in 1:length(new_outcome_vals)){

        indx <- grep(
          new_outcome_vals[i],
          solabels$variables$name,
          fixed=T
        )

        new_outcome_labs[i] <- solabels$variables$label[indx]
      }

      recode_vec <- levels(tab$outcome) %>% set_names(new_outcome_labs)

      tab %<>% mutate(outcome = fct_recode(outcome, !!!recode_vec))

    }

    # multiple exposures may lead to p-value columns with identical names
    # the columns of tab need to have unique names, but the table needs to
    # display identical names when it is printed. To make this work, I am
    # storing the vector of names with duplicated in tab_new_names

    # header behavior will change if there are interaction variables
    any_interaction_variables <-
      unique(analysis$.exposure) %>%
      map(~.x$term) %>%
      grepl("*", x = ., fixed=TRUE) %>%
      any()

    tab_new_names <- names(tab)

    tab_colname_replace_indx <- grep(pattern = "P Value", x = tab_new_names)

    # Force identical names and make the format work for header
    tab_new_names[tab_colname_replace_indx] <-
      tab_new_names[tab_colname_replace_indx] %>%
      map_chr(
        function(str){
          new_str <-
            strsplit(str, split = '._solo_.') %>%
            trim_list(rmv_inner_space = FALSE)

          new_str[1]<-" "
          paste(new_str, collapse='._solo_.')

        }
      )

    if(!any_interaction_variables){

      tab_new_names <- gsub(
        pattern = " ._solo_.",
        replacement = "",
        x = tab_new_names,
        fixed = TRUE)

    }

    blank_filler <- function(mat){

      counter=1
      for(i in 1:nrow(mat)){
        for(j in 1:ncol(mat)){

          if(mat[i,j]==''){
            mat[i,j]=letters[counter]
            counter=counter+1
          }

        }
      }

      mat

    }

    hdr=tab_new_names %>%
      str_split(pattern = '._solo_.', simplify = T) %>%
      blank_filler() %>%
      apply(2,rle) %>%
      map(
        .f=function(runs){
          header <- runs$lengths
          # headers of length 1 should be blank
          names(header)[header==1]=" "
          # headers of length 2 or more get the prefix names above
          names(header)[header>1]=runs$values[header>1] %>%
            dplyr::recode(!!!label_key)
          header
        }
      )

    # final value of hdr will be replaced by name_handler names
    hdr[length(hdr)]=NULL

    # Table names (swapping column names with labels)
    name_handler <- tibble(
      name=tab_new_names,
      desc=paste(tab[1,])
    ) %>%
      mutate(
        new_name = map_chr(name, grab_last, split='._solo_.'),
        new_name = case_when(
          !( desc %in% c(" ", 'desc', '1') ) ~
            paste(new_name, desc, sep = '<br/>N = '),
          desc == 1 ~ " ",
          TRUE ~ new_name
        ),
        new_name=dplyr::recode(new_name, !!!label_key)
      )


    # Now that the No. of participants row has been sucked into
    # the names of the table, it needs to be removed from tab
    tab %<>%
      dplyr::filter(
        table_row != 'No. of participants (%)'
      )

    # Grouping the rows

    # There are two grouping mechanics:
    # Inner groups -- descriptive or model-based stats
    # The index for these groups is equal to the runs of desc/ests
    inner_runs <- rle(tab$stat)

    inner_indx = inner_runs$lengths %>%
      set_names(
        inner_runs$values
      )

    n_outcomes <- length(unique(analysis$outcome))

    # Outer groups -- outcomes
    if(n_outcomes > 1){
      # If there is >1 outcome, then...
      outer_runs <- rle(as.character(tab$outcome))
      # Index for these groups is equal to runs of outcome values
      outer_indx = outer_runs$lengths %>%
        set_names(
          outer_runs$values
        )
    }

    # groups with descriptive statistics do not need a label
    names(inner_indx)[names(inner_indx)=='desc'] = " "
    names(inner_indx)[names(inner_indx)=='ests'] = ratio_label

    # Adding footnotes to models
    model_labs <- levels(analysis$formula)

    if("Unadjusted" %in% model_labs){

      model_labs = model_labs[-which(model_labs=='Unadjusted')]

    }

    model_indx <- map_int(
      model_labs, ~ min(which(tab$table_row==.x))
    )

    tab$table_row %<>% as.character()

    tab$table_row[model_indx] <- paste0(
      kableExtra::footnote_marker_symbol(1:length(model_labs)),
      tab$table_row[model_indx]
    )

    nrows = nrow(tab)

    tab %<>%
      set_names(
        name_handler$new_name
      ) %>%
      .[,-c(1,2)] %>%
      kable(
        align = c("l",rep("c",ncol(.)-1)),
        escape=FALSE
      ) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped","hover"),
        full_width = TRUE
      ) %>%
      kableExtra::add_footnote(
        label = solabels$footnote, notation = 'symbol'
      )

    for(h in length(hdr):1){

      tab %<>% kableExtra::add_header_above(
        header = hdr[[h]][-c(1,2)]
      )

    }

    if(n_outcomes > 1){
      tab %<>%
        add_indent(positions = 1:nrows) %>%
        kableExtra::pack_rows(
          index = outer_indx,
          indent = FALSE,
          label_row_css = "text-align: left; background-color: #D3D3D3;"
        ) %>%
        kableExtra::pack_rows(
          index = inner_indx,
          indent = FALSE
        )
    } else {
      tab %<>%
        kableExtra::pack_rows(
          index = inner_indx,
          indent = TRUE
        )
    }

    output$wide <- tab

  }


  output

}
