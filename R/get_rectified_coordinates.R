#'@title  get coordinates from rectified grid
#'@noRd
get_rectified_coordinates <- function(x, resolution, rounding = "nearest") {

  if (resolution == 0.5) {
    # don't round if it is a integer
    # don't round if decimal digit == 0.5
    # round down if first decimal digit is < 5 (0.5-)
    # round up if first decimal digit is > 5 (0.5+)
    has_decimal <- x != floor(x)
    if (has_decimal == TRUE) {
      rounded_value <- round(x, 1)  # First round to 1 decimal digit
      # Check if the rounded value is exactly 0.5
      if (abs(rounded_value - floor(rounded_value)) == 0.5) {
        z <- rounded_value  # If it's exactly 0.5, retain the rounded value
      } else {
        if (rounding == "nearest") {
          z <- round(x)  # Otherwise, round using the round() function
        } else if (rounding == "up") {
          z <- ceiling(x)
        } else if (rounding == "down") {
          z <- floor(x)
        }
      }
    } else {
      z <- x  # Return x without rounding if it's an integer
    }
  }
  return(z)
}
