library(stringr)
library(tidyverse)


#### expand indicators into separate columns for multiple-select questions ####
expand_indicators <- function(data, col_name, names_prefix = T, alt_prefix = NA, separator = ', ') {
  data <- data %>% separate_rows(!!sym(col_name), sep = separator) %>% 
    distinct %>%  # added this to eliminate duplicate selections due to replacement of "others"
    mutate(bool = TRUE)
  if (names_prefix) {
    if (is.na(alt_prefix)) {  # if no alt prefix supplied, just use col name
      prefix = paste(c(col_name, '_'), collapse = '')
    } else {
      prefix = paste(c(alt_prefix, '_'), collapse = '')
    }
    data <- data %>%
      pivot_wider(names_from = !!sym(col_name), values_from = bool, values_fill = FALSE,
                  names_prefix = prefix) 
  }
  else {
    data <- data %>%
      pivot_wider(names_from = !!sym(col_name), values_from = bool, values_fill = FALSE) 
  }
  return(data)
}


#### function that shows current average rankings per concept ####
show_rankings <- function(data) {
  results <- data %>% summarize(across(starts_with('claim_rank'), mean))
  results <- data.frame(t(results))
  colnames(results) <- 'avg_ranking'
  results <- results %>% arrange(avg_ranking)
  return(results)
}

#### table of counts of non-mutually exclusive indicator variables ####
indicator_counts_tbl <- function(data, regex, descending = T) {
  tbl <- data %>%
    select(matches(regex)) %>%
    summarise(across(everything(), ~sum(. == TRUE, na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "Column", values_to = "Count") %>%
    mutate(pct = Count / nrow(data))
  
  if (descending == T) {
    tbl <- tbl %>% arrange(desc(Count))
  }
  else {
    tbl <- tbl %>% arrange(Column)
  }
  
  return(tbl)
}

#### plot to show counts of a single-select variable ####
single_select_plot <- function(data, var_name, order_desc = T, preserve_levels = T, table = F) {
  # order_desc = order groups by descending total count
  plot_tbl <- data %>% group_by_at(var_name) %>% summarize(count = n())
  
  if (order_desc == T) {
    plot_tbl[var_name] <- factor(plot_tbl %>% pull(!!var_name),
                                 levels = unique((plot_tbl %>% pull(!!var_name))[order(plot_tbl$count, decreasing = T)]))
  }
  
  if (preserve_levels == T) {
    plot_tbl[[var_name]] <- factor(plot_tbl[[var_name]], levels = levels(data[[var_name]]))
  }
  
  plot_tbl <- plot_tbl %>% filter(!is.na(!!sym(var_name)))
  # plot_tbl <- plot_tbl %>% ungroup %>% mutate(total = sum(count)) %>% mutate(pct = round(count/total, 2)*100)
  nrows = nrow(data)
  plot_tbl <- plot_tbl %>% ungroup %>% mutate(total = nrows) %>% mutate(pct = round(count/total, 2)*100)
  
  label_pos <- max(plot_tbl$count) * 0.07
  
  if (table == T) {
    print(plot_tbl)
  }
  
  plot <- ggplot(plot_tbl, aes(x = !!sym(var_name), y = count)) + geom_bar(stat = 'identity', fill = 'orange') +
    geom_text(size = 3, aes(label = paste('n =', count)), vjust = 0.5, y = 1) +
    geom_text(size = 5, aes(label = paste(pct, '%', sep = '')), vjust = 0.5, y = label_pos) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  if (preserve_levels == T) {
    plot <- plot + scale_fill_discrete(drop = FALSE) +
      scale_x_discrete(drop = FALSE, labels = function(x) {str_wrap(x, width = 30)}) 
  }
  else {
    plot <- plot + scale_x_discrete(labels = function(x) {str_wrap(x, width = 30)})
  }
  
  return(plot)
}

#### facet single-select plot ####
# TODO: percentages are wrong b/c not calculated within segments
single_select_plot_facet <- function(data, var_name, segment_var_name, order_desc = T, preserve_levels = T, table = F) {
  # order_desc = order groups by descending total count
  plot_tbl <- data %>% group_by(across(all_of(c(var_name, segment_var_name)))) %>% summarize(count = n())
  
  if (order_desc == T) {
    plot_tbl[var_name] <- factor(plot_tbl %>% pull(!!var_name),
                                 levels = unique((plot_tbl %>% pull(!!var_name))[order(plot_tbl$count, decreasing = T)]))
  }
  
  if (preserve_levels == T) {
    plot_tbl[[var_name]] <- factor(plot_tbl[[var_name]], levels = levels(data[[var_name]]))
  }
  
  plot_tbl <- plot_tbl %>% filter(!is.na(!!sym(var_name)))
  plot_tbl <- plot_tbl %>% ungroup %>% mutate(total = sum(count)) %>% mutate(pct = round(count/total, 2)*100)
  
  plot <- ggplot(plot_tbl, aes(x = !!sym(var_name), y = count)) + geom_bar(stat = 'identity', fill = 'orange') +
    geom_text(size = 3, aes(label = paste('n =', count)), vjust = 1.5, y = 1) +
    geom_text(size = 5, aes(label = paste(pct, '%', sep = '')), vjust = 1.5, y = 3) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    facet_grid(cols = vars(segment))
  
  if (preserve_levels == T) {
    plot <- plot + scale_fill_discrete(drop = FALSE) +
      scale_x_discrete(drop = FALSE, labels = function(x) {str_wrap(x, width = 30)}) 
  }
  else {
    plot <- plot + scale_x_discrete(labels = function(x) {str_wrap(x, width = 30)})
  }
  
  if (table == T) {
    print(plot_tbl)
  }
  
  return(plot)
}

#### plot to show counts of a multi-select variable ####
multi_select_plot <- function(data, regex, drop_zero = T) {
  plot_tbl <- indicator_counts_tbl(data, regex)
  
  plot_tbl['Column'] <- plot_tbl['Column'] %>% sapply(function(x) {gsub(regex, '', x)})
  
  plot_tbl['Column'] <- factor(plot_tbl %>% pull(Column),
                               levels = unique((plot_tbl %>% pull(Column))[order(plot_tbl$Count, decreasing = T)]))
  if (drop_zero == T) {
    plot_tbl <- plot_tbl %>% filter(Count != 0)
  }
  
  plot_tbl$pct = plot_tbl$pct %>% sapply(FUN = function(x) {100*round(x, 2)})
  
  plot <- ggplot(plot_tbl, aes(x = Column, y = Count)) + 
    geom_bar(stat = 'identity', position = 'stack', fill = 'orange') + 
    theme(axis.text.x = element_text(vjust = 1, hjust = 0.5)) +
    # geom_text(size = 3, aes(label = paste('n =', Count), vjust = 1.5, y = 2)) +
    geom_text(size = 3, aes(label = paste(pct, '%', sep = '')), vjust = 1.5, y = 4)  # TODO working on this
    scale_x_discrete(labels = function(x) {str_wrap(x, width = 10)})
  
  return(plot)
  
}


#### stacked bar plot for a question that is asked across all concepts, to compare the concepts ####
# not done debugging
cross_concept_plot <- function(attribute_pivot) {
  plot <- ggplot(attribute_pivot, aes(x = concept, fill = rating)) + geom_bar(position = 'fill') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ylab ('Proportion')
  return(plot)
}

# thanks chatgpt
cross_concept_plot_labels <- function(attribute_pivot, plot = FALSE) {
  # Ensure rating is a factor with explicit levels (so stacking is consistent)
  # attribute_pivot$rating <- factor(attribute_pivot$rating, levels = sort(unique(attribute_pivot$rating)))
  
  # Step 1: Compute counts and proportions, respecting rating order
  plot_data <- attribute_pivot %>%
    count(concept, rating) %>%
    group_by(concept) %>%
    mutate(
      prop = n / sum(n),
      # Order by factor level to match ggplot stacking
      rating = factor(rating, levels = levels(attribute_pivot$rating)),
      cum_prop = cumsum(prop),
      label_pos = 1-(cum_prop - prop / 2)
    ) %>%
    ungroup()
  
  if (plot == T) {
    print(plot_data)}
  
  # Step 2: Plot
  plot <- ggplot(plot_data, aes(x = concept, y = prop, fill = rating)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(y = label_pos, label = scales::percent(prop, accuracy = 1)),
      color = "white", size = 3
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ylab('Proportion')
  
  return(plot)
}

## grouped version
cross_concept_plot_labels_grouped <- function(attribute_pivot, grouping_var, plot = FALSE) {
  # Ensure rating is a factor with explicit levels (so stacking is consistent)
  # attribute_pivot$rating <- factor(attribute_pivot$rating, levels = sort(unique(attribute_pivot$rating)))
  
  # Step 1: Compute counts and proportions, respecting rating order
  plot_data <- attribute_pivot %>% filter(!is.na(!!sym(grouping_var))) %>%
    count(concept, rating, !!sym(grouping_var)) %>%
    group_by_at(vars(concept, !!sym(grouping_var))) %>%
    mutate(
      prop = n / sum(n),
      # Order by factor level to match ggplot stacking
      rating = factor(rating, levels = levels(attribute_pivot$rating)),
      cum_prop = cumsum(prop),
      label_pos = 1-(cum_prop - prop / 2)
    ) %>%
    ungroup()
  
  if (plot == T) {
    print(plot_data)}
  
  # Step 2: Plot
  plot <- ggplot(plot_data, aes(x = concept, y = prop, fill = rating)) + facet_grid(rows = .~eval(parse(text = grouping_var))) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(y = label_pos, label = scales::percent(prop, accuracy = 1)),
      color = "white", size = 3
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ylab('Proportion')
  
  return(plot)
}


##### reorder numeric coded factors for china survey ####
## intended to set the column as equal to the output of this function
# factor(as.character(car_china$sl_appeal), levels = sort(unique(car_china$sl_appeal)))
china_setlevels <- function(data, col_name) {
  data <- as.data.frame(data)
  col <- data[[col_name]]
  levels <- unique(col)
  # newlevels <- sort(levels)
  newlevels <- 1:max(levels)
  print(newlevels)
  releveled <- factor(as.character(data[[col_name]]), levels = newlevels)
  return(releveled)
}

#### recode numeric coded factors as factors for china survey ####
## intended to set the data frame as equal to the output of this function
china_recode <- function(data, col_name, ordered_levels_vector) {
  old_levels <- levels(data[[col_name]])
  # if (length(ordered_levels_vector != length(old_levels))) {
  #   
  # }
  mapping <- setNames(ordered_levels_vector, old_levels)
  print(mapping)
  recoded <- data %>% mutate(!!col_name := recode(!!sym(col_name), !!!mapping))
  return(recoded)
}

#### china set levels and recode all in one ####
china_relevel_recode <- function(data, col_name, ordered_levels_vector) {
  # data[[col_name]] <- china_setlevels(data, col_name)
  # newdata <- china_recode(data, col_name, ordered_levels_vector)
  
  # order numeric levels
  data <- as.data.frame(data)
  col <- data[[col_name]]
  newlevels <- 1:max(length(ordered_levels_vector))
  # print(newlevels)
  data[[col_name]] <- factor(as.character(data[[col_name]]), levels = newlevels)
  print(data[[col_name]] %>% levels)
  
  # recode numeric levels
  old_levels <- levels(data[[col_name]])
  mapping <- setNames(ordered_levels_vector, old_levels)
  print(mapping)
  recoded <- data %>% mutate(!!col_name := recode(!!sym(col_name), !!!mapping))
  return(recoded)
}

make_transcript_table <- function(transcript_folder) {
  # transcript_folder <- 'sample_transcripts'
  files <- list.files(path = transcript_folder, pattern = "*", full.names=TRUE, recursive=FALSE)
  for (i in 1:length(files)) {
    filename <- files[i]
    raw <- read.csv(filename)
    if (i == 1) {  # if first file, get question names + initiate table
      question_names <- raw$Question
      table <- data.frame(matrix(ncol = (1 + length(question_names))))
      colnames(table) <- c('filename', question_names)
    }
    transcript <- raw['Transcript'] %>% as.vector %>% unlist
    row <- c(filename, transcript)
    table <- rbind(table, row)
  }
  table <- table[2:nrow(table),]
  return(table)
} 




