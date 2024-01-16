
#' note_distribution
#'
#' @return
#' @export
#'
#' @examples
note_distribution <- function(midi_file) {
  # extract midi notes
  midi <- readMidi(midi_file)
  notes <- getMidiNotes(midi)

  # extract key signature changes and bind them to the notes
  # we are assuming here that key signature does not differ between voices (or hands on the grand staff)
  key_signature_changes <- midi %>% filter(type == 59)
  notes$key_signature <- key_signature_changes[findInterval(notes$time, (key_signature_changes %>% filter(track == 1))$time),]$parameterMetaSystem

  # bind the chromatic scale degrees of the notes based on their keys
  notes$chromatic_degree_c <- (notes$note) %% 12 + 1
  key_signature_key <- list("C"= 0,
                            "C#" = 1,
                            "Db" = 1,
                            "D" = 2,
                            "D#" = 3,
                            "Eb" = 3,
                            "E" = 4,
                            "F" = 5,
                            "F#" = 6,
                            "Gb" = 6,
                            "G" = 7,
                            "Ab" = 8,
                            "G#" = 8,
                            "A" = 9,
                            "A#" = 10,
                            "Bb" = 10,
                            "B" = 11)
  notes$key_signature_letter <- (strsplit(notes$key_signature, " ") %>% unlist)[c(TRUE, FALSE)]
  key_signature_by_notes <- unlist(key_signature_key[notes$key_signature_letter])
  notes$chromatic_scale_degree <- (key_signature_by_notes + (notes$chromatic_degree_c - 1)) %%  12

  # bind the scale degrees of the notes based on diatonic keys

  }
