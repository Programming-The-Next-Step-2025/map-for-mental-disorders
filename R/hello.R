# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}


#' Add two numbers
#'
#' This function returns the sum of two numbers.
#'
#' @param a A numeric value.
#' @param b A numeric value.
#' @return The sum of \code{a} and \code{b}.
#' @export
#' @examples
#' add_a_and_b(3, 5)
add_a_and_b <- function(a, b) {
  a + b
}

#' Multiply two numbers
#'
#' This function multiplies two numbers together.
#'
#' @param x A numeric value.
#' @param y A numeric value.
#' @return The product of \code{x} and \code{y}.
#' @export
#' @examples
#' multiply_x_and_y(2, 4)
multiply_x_and_y <- function(x, y) {
  x * y
}
#' Convert to uppercase
#'
#' Converts a string to uppercase using stringr.
#'
#' @param x A character string.
#' @return A character string in uppercase.
#' @export
#' @importFrom stringr str_to_upper
#' @examples
#' to_uppercase("hello")
to_uppercase <- function(x) {
  stringr::str_to_upper(x)
}



# roxygen2::roxygenize()
# devtools::load_all()

