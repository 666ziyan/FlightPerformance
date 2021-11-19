library(ggplot2)
library(tidyverse)
library(patchwork)

missing_plot <- function(dataset, plot_option) {
  
  missing_patterns <- data.frame(is.na(dataset)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  if (plot_option == "count") {
    missing_patterns_ <- missing_patterns[order(missing_patterns$count),]
    missingpatterns <- missing_patterns_[, c(1:length(missing_patterns_)-1)] %>% 
      rownames_to_column('id') %>% 
      gather(key, value, -id)
    completeId <- 0
    for (rowId in 1:nrow(missing_patterns_)) {
      if (('TRUE' %in% missing_patterns_[rowId,]) == FALSE) {
        completeId <- rowId
      }
    }
    mps <- data.frame(is.na(dataset))
    missingpatterns$trueNum <- 0
    for (name in names(mps)) {
      c_t <- sum(unlist(mps[, name]) == 'TRUE')
      missingpatterns$trueNum <- ifelse(missingpatterns$key == name , c_t , missingpatterns$trueNum)
    }
    missingpatterns$complete <- ifelse(missingpatterns$id == completeId , "1" , "0")
    
    p1 <- ggplot(missingpatterns, aes(x=reorder(key, -as.numeric(trueNum)), y=reorder(id, as.numeric(id)), fill=value, alpha=complete), xlabel = "variable", ylabel = "missing pattern") +
      geom_tile(color = "white", show.legend = FALSE) +
      labs(x="variable", y='missing pattern') +
      scale_alpha_manual("", values=c(0.7, 1.2)) +
      theme(axis.text.x = element_text(size = rel(0.5), angle=90, hjust=1, vjust=1))
    
    missingpatterns2 <- missing_patterns_ %>% 
      rownames_to_column('index')
    p2 <- ggplot(data = missingpatterns2, aes(x = count, y = reorder(index, count))) + 
      geom_bar(fill='green', color='black', stat="identity") +
      labs(x="row count", y='')
    
    missingpatterns3 <- data.frame(is.na(dataset)) %>%
      group_by_all()
    vc <- c()
    for (name in names(missingpatterns3)) {
      ct <- sum(unlist(missingpatterns3[, name]) == 'TRUE')
      vc <- append(vc, ct)
    }
    df <- data.frame(names(missingpatterns3), vc)
    p3 <- ggplot(data = df, aes(x = reorder(df$names.missingpatterns3., -df$vc), y = df$vc)) + 
      geom_bar(fill='green', color='black', stat="identity") +
      labs(y="num rows missing", x='') +
      theme(axis.text.x = element_text(size = rel(0.5), angle=90, hjust=1, vjust=1))
    
    p3 + plot_spacer() + p1 + p2 + plot_layout(ncol = 2, heights = c(1, 3), widths = c(3, 1))
  } else if (plot_option == "percent") {
    missing_patterns_ <- missing_patterns[order(missing_patterns$count),]
    missingpatterns <- missing_patterns_[, c(1:length(missing_patterns_)-1)] %>% 
      rownames_to_column('id') %>% 
      gather(key, value, -id)
    completeId <- 0
    for (rowId in 1:nrow(missing_patterns_)) {
      if (('TRUE' %in% missing_patterns_[rowId,]) == FALSE) {
        completeId <- rowId
      }
    }
    mps <- data.frame(is.na(dataset))
    missingpatterns$trueNum <- 0
    for (name in names(mps)) {
      c_t <- sum(unlist(mps[, name]) == 'TRUE')
      missingpatterns$trueNum <- ifelse(missingpatterns$key == name , c_t , missingpatterns$trueNum)
    }
    missingpatterns$complete <- ifelse(missingpatterns$id == completeId , "1" , "0")
    
    p1 <- ggplot(missingpatterns, aes(x=reorder(key, -as.numeric(trueNum)), y=reorder(id, as.numeric(id)), fill=value, alpha=complete), xlabel = "variable", ylabel = "missing pattern") +
      geom_tile(color = "white", show.legend = FALSE) +
      labs(x="variable", y='missing pattern') +
      scale_alpha_manual("", values=c(0.7, 1.2)) +
      theme(axis.text.x = element_text(size = rel(0.5), angle=90, hjust=1, vjust=1))
    
    missingpatterns2 <- missing_patterns_ %>% 
      rownames_to_column('index')
    missingpatterns2$freq <- missingpatterns2$count / sum(missingpatterns2$count)
    p2 <- ggplot(data = missingpatterns2, aes(x = freq, y = reorder(index, freq))) + 
      geom_bar(fill='green', color='black', stat="identity") +
      labs(x="% row count", y='')
    
    missingpatterns3 <- data.frame(is.na(dataset)) %>%
      group_by_all()
    vc <- c()
    vc2 <- c()
    for (name in names(missingpatterns3)) {
      ct <- sum(unlist(missingpatterns3[, name]) == 'TRUE')
      vc <- append(vc, ct)
      ct2 <- sum(unlist(missingpatterns3[, name]) == 'FALSE')
      vc2 <- append(vc2, ct2)
    }
    df <- data.frame(names(missingpatterns3), vc, vc2)
    df$freq <- df$vc / (df$vc + df$vc2)
    p3 <- ggplot(data = df, aes(x = reorder(df$names.missingpatterns3., -df$freq), y = df$freq)) + 
      geom_bar(fill='green', color='black', stat="identity") +
      labs(y="% rows missing", x='') +
      theme(axis.text.x = element_text(size = rel(0.5), angle=90, hjust=1, vjust=1))
    
    p3 + plot_spacer() + p1 + p2 + plot_layout(ncol = 2, heights = c(1, 3), widths = c(3, 1))
  } else {
    print("Wrong plotting option!")
  }
}
