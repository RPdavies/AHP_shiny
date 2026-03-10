pair_key <- function(left_id, right_id) {
 ifelse(
  left_id <= right_id,
  paste(left_id, right_id, sep = "||"),
  paste(right_id, left_id, sep = "||")
 )
}

all_pairs_df <- function(ids) {
 if (length(ids) < 2) {
  return(data.frame(
   left_id = character(),
   right_id = character(),
   stringsAsFactors = FALSE
  ))
 }
 
 cmb <- t(combn(ids, 2))
 data.frame(
  left_id = cmb[, 1],
  right_id = cmb[, 2],
  stringsAsFactors = FALSE
 )
}

next_evaluation_id <- function(rv) {
 rv$evaluation_counter <- rv$evaluation_counter + 1L
 sprintf("eval_%04d", rv$evaluation_counter)
}

build_pcm <- function(item_df, ev_df) {
 if (nrow(item_df) < 2) {
  return(NULL)
 }
 
 ids <- item_df$id
 labels <- item_df$label
 
 pcm <- diag(1, nrow = length(ids), ncol = length(ids))
 rownames(pcm) <- labels
 colnames(pcm) <- labels
 
 all_pairs <- all_pairs_df(ids)
 
 if (nrow(ev_df) > 0) {
  for (i in seq_len(nrow(ev_df))) {
   row <- ev_df[i, , drop = FALSE]
   
   li <- match(row$left_id, ids)
   ri <- match(row$right_id, ids)
   
   if (is.na(li) || is.na(ri)) next
   
   s <- row$strength
   if (is.na(s) || s <= 0) s <- 1
   
   if (row$choice == "equal") {
    val <- 1
   } else if (row$choice == "left") {
    val <- s
   } else if (row$choice == "right") {
    val <- 1 / s
   } else {
    next
   }
   
   pcm[li, ri] <- val
   pcm[ri, li] <- 1 / val
  }
 }
 
 list(
  pcm = pcm,
  complete = nrow(ev_df) == nrow(all_pairs),
  n_done = nrow(ev_df),
  n_total = nrow(all_pairs)
 )
}

compute_weights_df <- function(pcm, item_col = "Item") {
 eig <- eigen(pcm)
 w <- Re(eig$vectors[, 1])
 w <- abs(w)
 w <- w / sum(w)
 
 out <- data.frame(
  Item = rownames(pcm),
  Weight = as.numeric(w),
  stringsAsFactors = FALSE
 )
 names(out)[1] <- item_col
 
 out <- out[order(out$Weight, decreasing = TRUE), , drop = FALSE]
 rownames(out) <- NULL
 out
}