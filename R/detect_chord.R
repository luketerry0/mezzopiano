#' detect_chord classifies a chord based on the midi note numbers, assuming the chord includes the root
#'
#' @param note_numbers an integer vector of the midi note numbers of the chord
#'
#' @return a list of useful properties about the chord
#' @export
#'
#' @examples
detect_chord <- function(note_numbers) {
  # standardize all notes to the lowest octave of midi numbers
  note_numbers <- note_numbers + 12 %% 12

  # enumerate all permutations of the chord, and choose the one with the maximum number of (major or minor) thirds
  perms<- combinat::permn(note_numbers)
  intervals <- lapply(perms, diff)
  thirds <-  intervals %>% lapply(\(x){sum(x %in% c(3, 4, -9, -8))}) %>% unlist
  best_perm <- perms[which.max(thirds)] %>% unlist

  # we assume that the root is the lowest note in the permutation which stacks thirds...
  root <- best_perm[1]
  intervals_above_root <- (best_perm - best_perm[1])[-1]
  includes_major_third <- 4 %in% intervals_above_root
  includes_minor_third <- 3 %in% intervals_above_root
  includes_perfect_fourth <- 6 %in% intervals_above_root
  includes_perfect_fifth <- 7 %in% intervals_above_root
  includes_major_seventh <- 11 %in% intervals_above_root
  includes_minor_seventh <- 10 %in% intervals_above_root

  # attempt to write the chord name
  root_name <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")[root %% 12 + 1]
  if (includes_minor_third && !includes_major_third){
    if (includes_major_seventh && !includes_minor_seventh){
      chord_symbol = "mM7"
    }else if (includes_minor_seventh && !includes_major_seventh){
      chord_symbol = "m7"
    }else if(includes_perfect_fourth){
      chord_symbol = "dim"
    }else{
      chord_symbol = "m"
    }
  }else if (includes_major_third && !includes_minor_third){
    if (includes_major_seventh && !includes_minor_seventh){
      chord_symbol = "M7"
    }else if (includes_minor_seventh && !includes_major_seventh){
      chord_symbol = "dom7"
    }else{
      chord_symbol = "M"
    }
  }

  chord_name <- paste0(root_name, chord_symbol)

  return(list(chord_name = chord_name, root = root_name, intervals_above_root = intervals_above_root))
}
