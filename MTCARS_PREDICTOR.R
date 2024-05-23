# IMPORT/EXPORT DATA ###########################################################
dfCars = mtcars
write.csv(dfCars, "dfCars.csv", row.names = TRUE)
# CONSTANTS ####################################################################
vDesc = c(mpg  = "Miles/(US) gallon",
          cyl  = "Number of cylinders",
          disp = "Displacement (cu.in.)",
          hp   = "Gross horsepower",
          drat = "Rear axle ratio",
          wt   = "Weight (1000 lbs)",
          qsec = "1/4 mile time",
          vs   = "Engine (0: V-shaped, 1: straight)",
          am   = "Transmission (0: auto, 1: manual)",
          gear = "Number of forward gears",
          carb = "Number of carburetors")
vClass = c(mpg  = "Target DV",
           cyl  = "Design",
           disp = "Design",
           hp   = "Measurement",
           drat = "Design",
           wt   = "Design",
           qsec = "Measurement",
           vs   = "Design",
           am   = "Design",
           gear = "Design",
           carb = "Design")
# FUNCTION TO ANALYZE CORRELATION ##############################################
AnalyzeCorrelation = function(nPVal, nCoeff, nDegree) {
  # Determine significance level based on p-value
  sSignif = ifelse(nPVal < 0.001, "highly significant",
                   ifelse(nPVal < 0.01, "very significant",
                          ifelse(nPVal < 0.05, "significant",
                                 "not significant")))

  # Determine the direction of the coefficient
  sDir = ifelse(nCoeff > 0, "positive",
                ifelse(nCoeff < 0, "negative", "zero"))

  # Determine the degree of correlation
  sDegree = ifelse(nDegree > 0.7, "strong",
                   ifelse(nDegree > 0.4, "moderate",
                          ifelse(nDegree > 0.1, "weak", "very weak")))

  # Return a list of the results
  return(list(Significance = sSignif, Direction = sDir, Degree = sDegree))
}


# FUNCTION TO GET CORRELATION, P-VALUE,SIGNIFICANCE,DIRECTION, DEGREE ##########
GetCorr = function(df, col1, col2) {
  # Check if columns exist in the dataframe
  if (!(col1 %in% names(df) && col2 %in% names(df))) {
    stop("One or both specified columns do not exist in the dataframe.")
  }

  # Get data from the specified columns
  data1 = df[[col1]]
  data2 = df[[col2]]

  # Convert ordinal factors to numeric if necessary
  if (is.ordered(data1)) {
    data1 = as.numeric(levels(data1))[data1]
  }
  if (is.ordered(data2)) {
    data2 = as.numeric(levels(data2))[data2]
  }

  # Perform Spearman correlation test
  test_result = suppressWarnings(cor.test(data1, data2, method = "spearman"))

  # Extract the correlation coefficient and p-value
  correlation = test_result$estimate
  p_value = test_result$p.value

  # Calculate the degree of correlation (can be modified based on your criteria)
  nDegree = abs(correlation)

  # Analyze correlation significance, direction, and degree
  analysis_results = AnalyzeCorrelation(p_value, correlation, nDegree)

  # Combine and return all results
  return(list(
    Correlation = correlation,
    P_Value = p_value,
    Significance = analysis_results$Significance,
    Direction = analysis_results$Direction,
    Degree = analysis_results$Degree
  ))
}

# FUNCTION TO GET IV CORRELATION ORDER AND ORDERED CORRELATION MATRIX ##########
GetCorrMatrix = function(df, dv) {
  # Ensure the dependent variable is in the dataframe
  if (!dv %in% names(df)) {
    stop("The dependent variable does not exist in the dataframe.")
  }

  # Create an empty matrix to store correlation values
  n = ncol(df)
  corr_matrix = matrix(nrow = n, ncol = n)
  rownames(corr_matrix) = colnames(corr_matrix) = names(df)

  # Compute pairwise correlations using GetCorr
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {  # Fill lower triangle and diagonal
      corr_result = GetCorr(df, names(df)[i], names(df)[j])
      corr_matrix[i, j] = corr_matrix[j, i] = corr_result$Correlation
      # Symmetric matrix
    }
  }

  # Move the DV to the first position
  cols_order = c(dv, setdiff(names(df), dv))
  corr_matrix = corr_matrix[cols_order, cols_order]

  # Reorder the remaining IV columns by the magnitude of their correlation with the DV
  iv_cols = setdiff(cols_order, dv)
  iv_order = iv_cols[order(abs(corr_matrix[dv, iv_cols]), decreasing = TRUE)]
  final_cols_order = c(dv, iv_order)
  corr_matrix = corr_matrix[final_cols_order, final_cols_order]

  # Prepare the results to return
  results = list(
    CorrelationMatrix = corr_matrix,
    ColumnOrder = final_cols_order
  )

  return(results)
}
# CONDITION MTCARS,GENERATE CORRELATION MATRIX,GET IV CORRELATION ORDER ########
GetCondDF  = function(df, dv) {
  MakeOrdsDF = function(df, ORDS_THRESHOLD = 9) {
    # ORDINALIZE VARS WITH LOW COUNTS OF UNIQUE INTEGRAL VALUES
    dfRet = df
    for (col_name in names(df)) {
      col_data = df[[col_name]]
      if (is.numeric(col_data) && all(col_data == floor(col_data))) {
        unique_values = unique(col_data)
        num_unique_values = length(unique_values)
        if (num_unique_values <= ORDS_THRESHOLD) {
          dfRet[[col_name]] = factor(col_data,
                                     levels = sort(unique_values),
                                     ordered = TRUE)
        }
      }
    }
    return(dfRet)
  }
  dfRet = MakeOrdsDF(df)
  oGetCorrMatrix = GetCorrMatrix(dfRet, dv)
  mCorr = oGetCorrMatrix$CorrelationMatrix
  vCorrOrder = oGetCorrMatrix$ColumnOrder
  dfRet = dfRet[, vCorrOrder]
  vSummaries = list()
  for (col_name in names(dfRet)[-1]) {
    if (is.ordered(dfRet[[col_name]])) {
      levels = levels(dfRet[[col_name]])
      counts = table(factor(dfRet[[col_name]], levels = levels))
      vSummaries[[col_name]] = data.frame(
        Levels = paste(levels, collapse=", "),
        Counts = I(list(counts)))  # Use I() to keep list structure in df col
    }
  }
  if (length(vSummaries) > 0) {
    dfSumm = do.call(rbind, vSummaries)
    rownames(dfSumm) = names(vSummaries)
  } else {
    dfSumm = data.frame()  # Return an empty dataframe if no vSummaries exist
  }
  results = list(df = dfRet,
                 mat = mCorr,
                 order = vCorrOrder,
                 dfSumm = dfSumm)
  return(results)
}
oGetCondDF = GetCondDF(dfCars,'mpg')
dfCars      = oGetCondDF$df
mCorrs      = oGetCondDF$mat
vCorrOrder    = oGetCondDF$order
rm(oGetCondDF)
# FUNCTIONS, DATAFRAMES FOR TABLES #############################################
GetClassTblDF = function(vClass) {
  dfRet = data.frame(Class = unique(vClass),
                     Variables = "",
                     stringsAsFactors = FALSE)
  for (i in 1:nrow(dfRet)) {
    class_name = dfRet$Class[i]
    variables = names(vClass)[vClass == class_name]
    dfRet$Variables[i] = toString(variables)
  }
  return(dfRet)
}
dfClassTbl = GetClassTblDF(vClass)
GetSummDF = function(df, Descriptions) {
  if (!is.data.frame(df)) { stop("df must be a data frame.") }
  # Create a dataframe with variable names and their types
  dfRet = data.frame(Type = ifelse(sapply(df, is.numeric), "num",
                                   ifelse(sapply(df, is.ordered),
                                          "ord", "other")),
                     stringsAsFactors = FALSE)
  rownames(dfRet) = names(df)  # Set rownames as variable names
  vNumStats = c("Min", "Median", "Mean", "Max", "Uniqs")
  vOrdStats = c("NumOrds", "Ords", "Counts")
  for (stat in c(vNumStats, vOrdStats)) { dfRet[stat] = NA }
  for (i in seq_along(df)) {
    var_name = names(df)[i]
    var_data = df[[var_name]]
    if (is.numeric(var_data)) {
      dfRet[var_name, vNumStats] = c(min(var_data, na.rm = TRUE),
                                     median(var_data, na.rm = TRUE),
                                     mean(var_data, na.rm = TRUE),
                                     max(var_data, na.rm = TRUE),
                                     length(unique(var_data)))
    } else if (is.ordered(var_data)) {
      levels_info = levels(var_data)
      counts_info = table(var_data)
      dfRet[var_name, vOrdStats] = c(length(levels_info),
                                     toString(levels_info),
                                     toString(counts_info))
    }
  }
  dfRet$Description = Descriptions[rownames(dfRet)]
  return(dfRet)
}
dfVarsSumm = GetSummDF(dfCars,vDesc)
GetOrderedTypeSummDF = function(df, type, order_by) {
  # Ensure df is a dataframe
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  # Process the dataframe, filtering to include only rows of the specified type
  dfRet = df %>%
    filter(Type == type) %>%
    select(-Type) %>%
    select_if(~any(!is.na(.)))  # Remove columns that are entirely NA

  # Determine the type of order_by to decide the ordering approach
  if (is.character(order_by) && length(order_by) == 1 && order_by %in% names(df)) {
    # Order by a column within the dataframe
    dfRet = dfRet %>%
      arrange(!!sym(order_by))
  } else if (is.character(order_by)) {
    # Assume order_by is a vector of row names or similar identifiers
    dfRet = dfRet %>%
      mutate(row_name = rownames(.)) %>%
      filter(row_name %in% order_by) %>%
      mutate(order = match(row_name, order_by)) %>%
      arrange(order) %>%
      `rownames<-`(., .$row_name) %>%
      select(-row_name, -order)
  } else {
    stop("order_by must be a valid column name or a character vector for row ordering.")
  }

  return(dfRet)
}
dfOrdsSummTbl = GetOrderedTypeSummDF(dfVarsSumm, "ord", "NumOrds")
dfNumsSummTbl = GetOrderedTypeSummDF(dfVarsSumm, "num", vCorrOrder)
GetIVModelOrdTblDF = function(mCorr, vIgnore) {
  # Ensure inputs are valid
  if (!is.matrix(mCorr) || is.null(colnames(mCorr))) {
    stop("mCorr must be a named matrix.")
  }
  if (!is.vector(vIgnore)) {
    stop("vIgnore must be a vector.")
  }

  # Identify the target dependent variable (first column of the matrix)
  dv = colnames(mCorr)[1]

  # Extract the correlation coefficients for the DV, removing the DV itself
  correlations = mCorr[dv, -1]

  # Classify the variables
  direct = names(correlations[correlations > 0])
  inverse = names(correlations[correlations < 0])
  not_best_iv = intersect(vIgnore, colnames(mCorr))

  # Sort variables by absolute value of their correlations
  direct = direct[order(-abs(correlations[direct]))]
  inverse = inverse[order(-abs(correlations[inverse]))]

  # Convert lists to comma-separated strings
  direct = paste(direct, collapse = ', ')
  inverse = paste(inverse, collapse = ', ')
  not_best_iv = paste(not_best_iv, collapse = ', ')

  # Create the dataframe with complete lists
  df = data.frame(
    Class = c("Target DV", "Direct", "Inverse", "Not Best IV"),
    Variables = c(dv, direct, inverse, not_best_iv),
    stringsAsFactors = FALSE
  )
  return(df)
}
dfIVModelOrdTbl = GetIVModelOrdTblDF(mCorrs,c('hp','qsec'))
# FUNCTIONS TO MAKE PLOTS ######################################################
GetDistribPlot = function(df, col, dfName = deparse(substitute(df))) {
  # Return Distribution Plot for Col in DF
  if (!col %in% names(df)) {
    stop("Column ", sQuote(col), " not in DF ", dQuote(dfName), ".")
  }
  vCol = df[[col]] # Extract column data
  if (is.numeric(vCol)) { # Numeric distribution plot
    p = ggplot(df, aes_string(x = col)) +
      geom_histogram(binwidth = diff(range(vCol, na.rm = TRUE)) / 30) +
      theme_minimal() +
      labs(title = paste("Distribution of", col), x = col, y = "Count")
  } else if (is.factor(vCol)) { # Factor distribution plot
    p = ggplot(df, aes_string(x = col, fill = col)) +
      geom_bar() +
      labs(title = paste("Distribution of", col), x = col, y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else {
    stop("Data type '", typeof(vCol), "' of column ",
         sQuote(col), " is not supported for distribution plotting.")
  }
  return(p)
}
# FUNCTIONS AND VARIABLES FOR CORRELATION FIGURES ##############################
remove_from_vector = function(vector, remove_values) {
  # Remove specified values from the vector
  filtered_vector = vector[!vector %in% remove_values]
  return(filtered_vector)
}
remove_from_matrix = function(matrix, remove_values) {
  # Find indices of the values to remove from the matrix row and column names
  indices_to_remove = which(rownames(matrix) %in% remove_values | colnames(matrix) %in% remove_values)

  # Remove the corresponding rows and columns
  updated_matrix = matrix[-indices_to_remove, -indices_to_remove]
  return(updated_matrix)
}
GetMaxSquarePlotDims = function(n_rows, n_cols) {
  min(inPrtW / n_cols, inPrtH / n_rows)
}
GetCorrPlot = function(nCorr, sTitle) {
  if (is.na(nCorr) || is.nan(nCorr) || abs(nCorr) > 1) {
    return(ggplot() + annotate("text", x = 0, y = 0,
                               label = "Not plotable",
                               hjust = 0.5, vjust = 0.5))
  }

  dfPlot = data.frame(x = c(-1, 1), y = c(-1, 1) * nCorr)
  pltRet = ggplot(dfPlot, aes(x = x, y = y)) +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "blue") +
    geom_abline(intercept = 0, slope = -1, linetype = "dotted", color = "blue") +
    coord_fixed(ratio = 1) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    annotate("text", x = 0, y = 1, label = sTitle,
             hjust = 0.5, vjust = 1.1, size = 4, color = "black") +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "mm"))

  return(pltRet)
}
GetIndVarNamesAndIndices = function(nColDV, mCorrs) {
  if (!is.matrix(mCorrs) || nColDV < 1 || nColDV >= ncol(mCorrs)) {
    stop("Invalid input: Ensure mCorrs is a matrix and nColDV is a valid column index.")
  }
  nColFirstIV = nColDV + 1
  vIdx = (nColFirstIV):ncol(mCorrs)
  vNam = colnames(mCorrs)[vIdx]
  vVal = mCorrs[nColDV,vIdx]
  mOrig = data.frame(nam = vNam, idx = vIdx, val = vVal,
                     stringsAsFactors = FALSE)
  mReord = mOrig[order(abs(mOrig$val), decreasing = TRUE), ]  # Use abs() for sorting
  list(OrigNam = mOrig$nam,
       OrigIdx = mOrig$idx,
       OrigVal = mOrig$val,
       ReordNam = mReord$nam,
       ReordIdx = mReord$idx,
       ReordVal = mReord$val)
}
CheckValidCorrMatrixIdx = function(n, m) {
  # Check if n is a valid row index
  if (n < 1 || n > nrow(m)) {
    stop(sprintf("%d: Invalid Row Idx. Must be between 1 and %d.", n, nrow(m)))
  }
  # Check if n is also a valid column index
  if (n < 1 || n > ncol(m)) {
    stop(sprintf("%d: Invalid Col Idx. Must be between 1 and %d.", n, ncol(m)))
  }
  # If both checks are passed, return TRUE
  return(TRUE)
}
GetIndVarPlotRow = function(mCorrs, nColDV) {
  if (!CheckValidCorrMatrixIdx(nColDV, mCorrs)) {
    stop("Invalid column index for the matrix.")
  }
  oIndVarNamIdxVal = GetIndVarNamesAndIndices(nColDV, mCorrs)
  vPlots = lapply(seq_along(oIndVarNamIdxVal$ReordIdx), function(i) {
    idx = oIndVarNamIdxVal$ReordIdx[[i]]
    single_plot = GetCorrPlot(mCorrs[nColDV, idx], rownames(mCorrs)[idx])
    return(single_plot)
  })

  # Prepare the rho label as a ggplot object
  dep_var_name = rownames(mCorrs)[nColDV]  # Dependent variable name
  #rho_label = paste('$\\rho(', colnames(mCorrs)[nColDV], ')$', sep='')
  rho_label = paste("cor(", dep_var_name, ")", sep = "")
  #rho_label = expression(paste("rho(", dep_var_name, ")", sep = ""))
  #rho_label = expression(rho[.(dep_var_name)])
  rho_plot = ggplot() +
    geom_text(aes(x = 0, y = 0), label = rho_label, size = 2.9,
              hjust = 0.5, vjust = 0.5) +
    theme_void() +
    theme(plot.margin = margin(t = 0, b = 0, l = 0, r = 0))

  vPlots = c(list(rho_plot), vPlots)

  # Determine the number of empty plots needed to fill the row
  total_columns = length(colnames(mCorrs))
  num_empty_plots = total_columns - length(vPlots)
  empty_plots = replicate(num_empty_plots, ggplot() + theme_void(), simplify = FALSE)

  full_row = c(vPlots, empty_plots)
  # cat("Debug: DV =", dep_var_name,
  #     " | Number of plots:", length(vPlots) - 1,  # Exclude  rho plot
  #     " | Number of blank plots:", num_empty_plots, "\n")
  return(full_row)
}
GetAllIndVarPlotRows = function(mCorrs) {
  if (!is.matrix(mCorrs) || is.null(colnames(mCorrs))) {
    stop("mCorrs must be a named matrix.")
  }
  all_plot_rows = list()
  for (nColDV in 1:(ncol(mCorrs) - 1)) {
    current_row = GetIndVarPlotRow(mCorrs, nColDV)
    all_plot_rows = c(all_plot_rows, current_row)
  }
  return(all_plot_rows)
}
ArrangeAndResizeSquarePlots = function(plots, n_rows, n_cols) {
  # Use patchwork to combine plots
  plot_grid = patchwork::wrap_plots(plots, ncol = n_cols)
  plot_grid + patchwork::plot_layout(heights = rep(GetMaxSquarePlotDims(n_rows, n_cols), n_rows))
}
vAllDepVarPlotRows = GetAllIndVarPlotRows(mCorrs)
vNotIndVars = c('hp','qsec')
vCorrOrder01 = remove_from_vector(vCorrOrder,vNotIndVars)
mCorrsDepVarAndGoodIVs = remove_from_matrix(mCorrs, vNotIndVars)
vAllDepVarPlotRows01 = GetAllIndVarPlotRows(mCorrsDepVarAndGoodIVs)
mCorrsNoDepVar = remove_from_matrix(mCorrs, c('mpg'))

# FUNCTION TO MAKE CORRELATION GROUPS TBL DF ###################################
GetCorrGrps = function(mCorrs, Threshold = 0.7) {
  # Convert correlation matrix to a distance matrix
  dist_mat = as.dist(1 - abs(mCorrs))
  # Perform hierarchical clustering
  hc = hclust(dist_mat, method = "complete")
  # Cut tree to form groups based on the threshold
  clusters = cutree(hc, h = 1 - Threshold)
  # Extract variable names
  vars = rownames(mCorrs)
  # Initialize a list to store each group's data
  cluster_groups = list()
  # Iterate through each unique cluster
  for (k in unique(clusters)) {
    members = vars[clusters == k]
    # Ensure there are at least two members to form correlations
    if (length(members) > 1) {
      # Create a matrix of all pairwise combinations
      combns = combn(members, 2, simplify = FALSE)
      # Extract correlations for each combination
      correlations = sapply(combns, function(pair) mCorrs[pair[1], pair[2]])
      # Collect information about pairs and their correlations
      pairs_info = paste(sapply(combns, paste, collapse = "/"),
                          sprintf("(%.2f)", correlations), collapse = "; ")
      # Create a data frame for this group
      df = data.frame(
        #group_id = paste("Group", k, sep = "_"),
        members = paste(members, collapse = ", "),
        correlations = pairs_info,
        stringsAsFactors = FALSE
      )
      # Store the data frame in the list
      cluster_groups[[k]] = df
    }
  }
  # Combine all group data frames into a single data frame
  if (length(cluster_groups) > 0) {
    final_df = do.call(rbind, cluster_groups)
  } else {
    final_df = data.frame(group_id = character(),
                           members = character(),
                           correlations = character(),
                           stringsAsFactors = FALSE)
  }
  return(final_df)
}
mCorrsNoDepVar = remove_from_matrix(mCorrs, c('mpg'))
dfCorrGrpsTbl = GetCorrGrps(mCorrsNoDepVar)
# FUNCTION TO COMPARE MODELS ###################################################
compare_two_models = function(model1, model2,
                               aic_threshold = 2,
                               bic_threshold = 6,
                               adj_r2_threshold = 0.01) {
  # Make sure input models are fitted model objects
  if (class(model1) != "lm" || class(model2) != "lm") {
    stop("Both inputs must be fitted model objects from 'lm' or similar.")
  }
  aic1 = AIC(model1)
  aic2 = AIC(model2)
  bic1 = BIC(model1)
  bic2 = BIC(model2)
  adj_r21 = summary(model1)$adj.r.squared
  adj_r22 = summary(model2)$adj.r.squared
  aic_diff = abs(aic1 - aic2)
  bic_diff = abs(bic1 - bic2)
  adj_r2_diff = abs(adj_r21 - adj_r22)
  cat("Comparison of Two Models:\n")
  cat("Difference in AIC: ", aic_diff, "\n")
  cat("Difference in BIC: ", bic_diff, "\n")
  cat("Difference in Adjusted R-squared: ", adj_r2_diff, "\n")
  message = ifelse(
    aic_diff < aic_threshold && bic_diff < bic_threshold && adj_r2_diff < adj_r2_threshold,
    "\nBoth models perform similarly based on the criteria.\n",
    "\nThere is a significant difference between the models.\n"
  )
  cat(message)
}
# MODELS FOR COMPARISON ########################################################
full_model = lm(mpg ~ ., data = mtcars)
intercept_only = lm(mpg ~ 1, data = mtcars)
step_model = step(intercept_only,
                   direction = "both",
                   scope = formula(full_model),
                   trace = 0)
mdlWtCylHp = lm(mpg ~ wt + cyl + hp, data = mtcars)
mdlWtCyl = lm(mpg ~ wt + cyl,      data = mtcars)
predWtCylHp = predict(mdlWtCylHp, newdata = mtcars)
predWtCyl = predict(mdlWtCyl, newdata = mtcars)
comparison_df = data.frame(Car = rownames(mtcars),
                            Actual = mtcars$mpg,
                            mdlWtCylHp = predWtCylHp,
                            mdlWtCyl = predWtCyl)
rownames(comparison_df) = comparison_df$Car
comparison_df$Car = NULL  # remove Car column
