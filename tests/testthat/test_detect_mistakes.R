context("Check code with calls")
a <<- function(x) x
b <<- function(x) x

test_that("detect_mistakes detects surplus code", {
  # function
  user <-     quote(a(b(1)))
  solution <- quote(b(1))
  expect_equal(
               detect_mistakes(user, solution),
               list(message = wrong_call(this = user, that = solution),
                    category = category_mistake(solution))
               )

  user <-     quote(b(b(1)))
  solution <- quote(b(1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user[[2]], that = solution[[2]], enclosing_call = user),
         category = category_mistake(user))
  )

  user <-     quote(a(b(1)))
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user[[2]], that = solution[[2]], enclosing_call = user),
         category = category_mistake(user))
  )

  # non-function
  user <-     quote(1(a(1))) # nolint
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user, that = solution),
         category = category_mistake(solution))
  )

  # internal atomic
  # arguments
  user <-     quote(b(1))
  solution <- quote(b())
  expect_equal(
    detect_mistakes(user, solution),
    list(message = surplus_argument(this_call = user, this = user[[2]]),
         category = category_mistake(user[[2]]))
  )

  # internal non-function
  user <-     quote(a(1(1))) # nolint
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user[[2]], that = solution[[2]], enclosing_call = user),
         category = category_mistake(user))
  )
})

test_that("detect_mistakes detects missing code", {

  # function
  user <-     quote(b(1))
  solution <- quote(a(b(1)))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user, that = solution),
         category = category_mistake(solution))
  )


  # non-function
  user <-     quote(1(1)) # nolint
  solution <- quote(a(b(1)))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user, that = solution),
         category = category_mistake(solution))
  )

  # internal atomic - NEEDS TO CATCH UNNAMED ARGUMENT HANDLING
  user <-     quote(a())
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = missing_argument(this_call = user, that_name = "x"),
         category = category_mistake(user))
  )

  # internal function
  user <-     quote(a(1))
  solution <- quote(a(b(1)))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = quote(1), that = quote(b()), enclosing_call = user),
         category = category_mistake(user))
  )

  # internal non-function would not appear in a solution

})

test_that("detect_mistakes detects mis-matched code", {

  # function
  user <-     quote(b(1))
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user, that = solution),
         category = category_mistake(solution))
  )

  # non-function
  user <-     quote(1(1)) # nolint
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user, that = solution),
         category = category_mistake(solution))
  )

  # internal atomic
  user <-     quote(a(1))
  solution <- quote(a(2))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = quote(1), that = quote(2), enclosing_call = user),
         category = category_mistake(user))
  )

  # internal function
  user <-     quote(a(b(1)))
  solution <- quote(a(c(1)))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user[[2]], that = solution[[2]], enclosing_call = user),
         category = category_mistake(solution[[2]]))
  )

  # internal non-function
  user <-     quote(a(1(1))) # nolint
  solution <- quote(a(b(1)))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user[[2]], that = solution[[2]], enclosing_call = user),
         category = category_mistake(solution[[2]]))
  )

})

test_that("detect_mistakes works with atomic solutions", {

  user <-     quote(2)
  solution <- quote(1)
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = "2", that = quote(1)),
         category = category_mistake(user))
  )

  # function
  user <-     quote(a(1))
  solution <- quote(1)
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user, that = solution),
         category = category_mistake(user))
  )

  user <-     quote(a())
  solution <- quote(1)
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = "a()", that = quote(1)),
         category = category_mistake(solution))
  )

  user <-     quote(a(1))
  solution <- quote(pi)
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user, that = solution),
         category = category_mistake(solution))
  )

  # non-function
  user <-     quote(pi(1))
  solution <- quote(pi)
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user, that = solution),
         category = category_mistake(solution))
  )

  # internal atomics, functions, non-functions, infixes,
  # and pipes will not matter if the above tests pass.
  # Why? Because checking will stop at the initial call
  # because it is not an atomic.

})

test_that("detect_mistakes works with infix operators", {

  user <- quote(123)
  solution <- quote(x <- sample(1:6, size = 1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user, that = solution),
         category = category_mistake(solution))
  )
  
  # changing direction of assign should still work
  user <- quote(123)
  solution <- quote(sample(1:6, size = 1) -> x)
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user, that = solution),
         category = category_mistake(solution))
  )
  
  # other variants of assign like <<- should also work
  
  user <- quote(123)
  solution <- quote(x <<- sample(1:6, size = 1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user, that = solution),
         category = category_mistake(solution))
  )
  
  user <- quote(123)
  solution <- quote(sample(1:6, size = 1) ->> x)
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user, that = solution),
         category = category_mistake(solution))
  )
  
  # call vs assign
  user <- quote(sample(1:6, size = 1))
  solution <- quote(x <- sample(1:6, size = 1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user, that = solution),
         category = category_mistake(solution))
  )
  
#   # surplus
#   user <-     quote(b(1 + 2))
#   solution <- quote(b(1))
#   expect_equal(
#                detect_mistakes(user, solution)
#                ,
#                wrong_value("1 + 2", quote(1))
#                )
#
#   # missing
#   user <-     quote(sqrt(1))
#   solution <- quote(sqrt(1 + 2))
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = quote(1), that = "1 + 2")
#   )
#
#   user <-     quote(sqrt(1))
#   solution <- quote(sqrt(1 + 2 + 3))
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = quote(1), that = "1 + 2 + 3")
#   )
#
#   user <-     quote(sqrt(1 + 2))
#   solution <- quote(sqrt(1 + 2 + 3))
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "+ 2", that = "+ 3")
#   )
#
#   user <-     quote(sqrt(1 + 3))
#   solution <- quote(sqrt(1 + 2 + 3))
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "1", that = "1 + 2")
#   )
#
#   # internal infix
#   user <-     quote(a(1 + 2))
#   solution <- quote(a(1 + 3))
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "+ 2", that = "+ 3")
#   )
#
#   user <-     quote(a(1 + 2 + 4))
#   solution <- quote(a(1 + 3 + 4))
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "+ 2", that = "+ 3")
#   )
#
#   user <-     quote(a(1 + 2 + 4))
#   solution <- quote(a(1 + 3 + 5))
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "+ 4", that = "+ 5")
#   )
#
#   user <-     quote(a(2 + 1))
#   solution <- quote(a(3 + 1))
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "2", that = "3")
#   )
#
#   user <-     quote(a(1 + 1))
#   solution <- quote(a(1 - 1))
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "1 + 1", that = "1 - 1")
#   )
#
#   user <-     quote(a(1 + 1 + 1))
#   solution <- quote(a(1 - 1 + 1))
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "1 + 1", that = "1 - 1")
#   )
#
#   # surplus
#   user <-     quote(1 + 2)
#   solution <- quote(1)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value("1 + 2", quote(1))
#   )
#
#   # missing
#   user <-     quote(1)
#   solution <- quote(1 + 2)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = quote(1), that = "1 + 2")
#   )
#
#   user <-     quote(1)
#   solution <- quote(1 + 2 + 3)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = quote(1), that = "1 + 2 + 3")
#   )
#
#   user <-     quote(1 + 2)
#   solution <- quote(1 + 2 + 3)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "+ 2", that = "+ 3")
#   )
#
#   user <-     quote(1 + 3)
#   solution <- quote(1 + 2 + 3)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "1", that = "1 + 2")
#   )
#
#   # internal infix
#   user <-     quote(1 + 2)
#   solution <- quote(1 + 3)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "+ 2", that = "+ 3")
#   )
#
#   user <-     quote(1 + 2 + 4)
#   solution <- quote(1 + 3 + 4)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "+ 2", that = "+ 3")
#   )
#
#   user <-     quote(1 + 2 + 4)
#   solution <- quote(1 + 3 + 5)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "+ 4", that = "+ 5")
#   )
#
#   user <-     quote(2 + 1)
#   solution <- quote(3 + 1)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "2", that = "3")
#   )
#
#   user <-     quote(1 + 1)
#   solution <- quote(1 - 1)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "1 + 1", that = "1 - 1")
#   )
#
#   user <-     quote(1 + 1 + 1)
#   solution <- quote(1 - 1 + 1)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "1 + 1", that = "1 - 1")
#   )
#
#   # function
#   user <-     quote(a(1))
#   solution <- quote(1 + pi)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "a(1)", that = "1 + pi")
#   )
#
#   user <-     quote(b(1))
#   solution <- quote(b(1) + 2)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "b(1)", that = "b(1) + 2")
#   )
#
#   user <-     quote(b(1))
#   solution <- quote(b(1) + a(2))
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "b(1)", that = "b(1) + a(2)")
#   )
#
#   # non-function
#   user <-     quote(pi(1))
#   solution <- quote(1 + pi)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "pi(1)", that = "1 + pi")
#   )
#
#   user <-     quote(1(1)) # nolint
#   solution <- quote(b(1) + 2)
#   expect_equal(
#     detect_mistakes(user, solution)
#     ,
#     wrong_value(this = "1(1)", that = "b(1) + 2")
#   )
#
#   # internal atomics, functions, non-functions, infixes,
#   # and pipes will not matter if the above tests pass.
#   # Why? Because checking will stop at the initial call
#   # because it is not an infix.
# 
})

test_that("detect_mistakes works with pipes", {

  # internal pipe
  user <-     quote(b(1 %>% abs()))
  solution <- quote(b(1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user[[2]][[3]], that = solution[[2]], enclosing_call = user),
         category = category_mistake(user))
  )

  user <-     quote(sqrt(1))
  solution <- quote(sqrt(1 %>% log()))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user[[2]], that = solution[[2]][[3]], enclosing_call = user),
         category = category_mistake(user))
  )

  user <-     quote(sqrt(1))
  solution <- quote(sqrt(1 %>% log() %>% abs())) # nolint
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user[[2]], that = solution[[2]][[3]], enclosing_call = user),
         category = category_mistake(user))
  )

  user <-     quote(sqrt(1 %>% log()))
  solution <- quote(sqrt(1 %>% log() %>% abs())) # nolint
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user[[2]][[3]], that = solution[[2]][[3]], enclosing_call = user),
         category = category_mistake(solution[[2]][[3]]))
  )

  ## TODO infix operator
  # user <-     quote(sqrt(1 + 2))
  # solution <- quote(sqrt(1 + 2 %>% log()))
  # expect_equal(
  #   detect_mistakes(user, solution)
  #   ,
  #   wrong_value(this = "+ 2", that = "+ log()")
  # )

  # internal pipe
  user <-     quote(a(2 %>% abs()))
  solution <- quote(a(2 %>% log()))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user[[2]][[3]], that = solution[[2]][[3]], enclosing_call = user),
         category = category_mistake(solution[[2]][[3]]))
  )

  # DOES MESSAGE AUTOMATICALLY UNPIPE INNER ARGUMENTS?
  # user <-     quote(a(2 %>% abs() %>% sqrt())) # nolint
  # solution <- quote(a(2 %>% log() %>% sqrt())) # nolint
  # expect_equal(
  #   detect_mistakes(user, solution),
  #   wrong_call(this = user[[2]][[2]][[3]], that = solution[[2]][[2]][[3]], enclosing_call = user[[2]])
  # )

  # TODO infix operator
  # user <-     quote(a(2 %>% abs()))
  # solution <- quote(a(2 + log(1)))
  # expect_equal(
  #   detect_mistakes(user, solution)
  #   ,
  #   wrong_value(this = "abs(2)", that = "2 + log(1)")
  # )

  # external pipe
  user <-     quote(1 %>% abs())
  solution <- quote(1)
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user, that = solution),
         category = category_mistake(solution))
  )

  user <-     quote(1)
  solution <- quote(1 %>% log())
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = quote(1), that = quote(log())),
         category = category_mistake(user))
  )

  user <-     quote(1)
  solution <- quote(1 %>% log() %>% abs()) # nolint
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = quote(1), that = quote(abs())),
         category = category_mistake(user))
  )

  user <-     quote(1 %>% log())
  solution <- quote(1 %>% log() %>% abs()) # nolint
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user[[3]], that = quote(abs())),
         category = category_mistake(quote(abs())))
  )

  ## TODO infix operator
  # user <-     quote(1 + 2)
  # solution <- quote(1 + 2 %>% log())
  # expect_equal(
  #   detect_mistakes(user, solution)
  #   ,
  #   wrong_value(this = "+ 2", that = "+ log()")
  # )

  # internal pipe
  user <-     quote(2 %>% abs())
  solution <- quote(2 %>% log())
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = unpipe(user), that = "log()"),
         category = category_mistake(solution[[3]]))
  )

  user <-     quote(2 %>% abs() %>% sqrt()) # nolint
  solution <- quote(2 %>% log() %>% sqrt()) # nolint
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = unpipe(unpipe(user)[[2]]),
                that = unpipe(unpipe(solution)[[2]]),
                enclosing_call = user),
         category = category_mistake(unpipe(unpipe(solution)[[2]]))
    )
  )

  ## TODO need to look into infix operators
  # user <-     quote(2 %>% abs())
  # solution <- quote(2 + log(1))
  # expect_equal(
  #   detect_mistakes(user, solution)
  #   ,
  #   # wrong_value(this = "abs(2)", that = "2 + log(1)")
  #   missing_argument(this_call = quote(`+`()), that = quote(log()))
  # )

  user <-     quote(b(1))
  solution <- quote(b(1) %>% a())
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user, that = "a()"),
         category = category_mistake(solution[[3]]))
  )

})

test_that("detect_mistakes handles argument names correctly", {
  user <-     quote(c(x = a(b(1))))
  solution <- quote(c(x = b(1)))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user[[2]],
                this_name = names(as.list(user))[2],
                that = solution[[2]],
               enclosing_call = user),
         category = category_mistake(solution[[2]]))
  )

  user <-     quote(b(x = 1))
  solution <- quote(b(1))
  expect_null(
    detect_mistakes(user, solution)
  )

  user <-     quote(b(1))
  solution <- quote(b(x = 1))
  expect_null(
    detect_mistakes(user, solution)
  )

  user <-     quote(b(y = 1))
  solution <- quote(b(x = 1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = surplus_argument(this_call =  quote(b()),
                     this = quote(1),
                     this_name = "y"),
         category = category_mistake(quote(b())))
  )

  user <-     quote(b(y = a(1)))
  solution <- quote(b(1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = surplus_argument(this_call =  quote(b()),
                     this = "a()",
                     this_name = "y"),
         category = category_mistake(quote(b())))
  )

  test_fn <<- function(x, y = 1, z = FALSE, ...) {return(1)}

  user <-     quote(test_fn(1:10, a = 1, z = TRUE))
  solution <- quote(test_fn(1:10, b = 1, z = TRUE))
  expect_equal(
    detect_mistakes(user, solution),
    # wrong_value(this = quote(1),
    #             this_name = "cut",
    #             that = quote(1),
    #             that_name = "trim")
    list(message = surplus_argument(this_call = quote(test_fn()), this = quote(1), this_name = "a"),
         category = category_mistake(quote(test_fn())))
  )

  user <-     quote(test_fn(1:10, a = 1, z = TRUE))
  solution <- quote(test_fn(1:10, 1, z = TRUE))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = surplus_argument(this_call = quote(test_fn()), this = quote(1), this_name = "a"),
         category = category_mistake(quote(test_fn())))
  )

  # This user code looks correct (and runs!) but na.rm is an argument passed to
  # ... that does not appear in the solution, and so should be flagged wrong.
  user <-     quote(mean(1:10, cut = 1, na.rm = TRUE))
  solution <- quote(mean(1:10, TRUE, cut = 1))
  expect_equal(
    detect_mistakes(user, solution),
    # wrong_value(this = quote(1),
    #             this_name = "cut",
    #             that = quote(TRUE))
    list(message = surplus_argument(this_call = quote(mean()), this = quote(TRUE), this_name = "na.rm"),
         category = category_mistake(quote(mean())))
  )

})

test_that("detect_mistakes handles weird cases", {

  user <-     quote(sum(sum(1, 2), 3))
  solution <- quote(sum(1, 2, 3))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this =  user[[2]], that = solution[[2]], enclosing_call = user),
         category = category_mistake(user))
  )

  user <-     quote(sum(1, 2))
  solution <- quote(sum(1, 2, 3))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = missing_argument(this_call =  quote(sum()),
                     that_name = quote(3)),
         category = category_mistake(quote(sum())))
  )

})


test_that("detect_mistakes checks the call first", {

  user <-     quote(0 + sqrt(log(2)))
  solution <- quote(sqrt(log(2)))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_call(this = user, that = solution),
         category = category_mistake(solution[[1]])
    )
  )

})

test_that("detect_mistakes does not throw error for unused argument", {

  a <- function(x) x
  user <-     quote(a(1, y = 2))
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = surplus_argument(this_call = quote(a()), this = quote(2), this_name = "y"),
         category = category_mistake(quote(a())))
  )

})

test_that("detect_mistakes does not throw error for multiple matches of argument", {

  z <<- function(x, ya = 1, yb = 2) x
  user <-     quote(z(1, y = 2))
  solution <- quote(z(1, ya = 2))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = bad_argument_name(this_call = user,
                      this = user[[3]],
                      this_name = names(as.list(user)[3])),
         category = category_mistake(user))
  )

})

test_that("detect_mistakes does not throw error for multiple matches of formal", {

  zz <<- function(x, yab = 1, ...) x
  user <-     quote(zz(1, y = 2, ya = 3))
  solution <- quote(zz(1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = too_many_matches(this_call = user, that_name = "yab"),
         category = category_mistake(user))
  )

})

test_that("detect_mistakes handles duplicated argument names", {

  dd <<- function(a) a
  user <-     quote(dd(a = 1, a = 2))
  solution <- quote(dd(a = 1))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = duplicate_name(this_call = user, this_name = "a"),
         category = category_mistake(user))
  )

})

test_that("detect_mistakes does not return correct prematurely", {

  j <<- function(...) 1
  user <- quote(j(x = a(1), y = a(2)))
  solution <- quote(j(x = a(x = 1), y = a(3)))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = user[[3]][[2]], that = solution[[3]][[2]], enclosing_call = user[[3]]),
         category = category_mistake(solution[[3]][[2]]))
  )

})




test_that("detect_mistakes works with multiple lines", {

  user <- rlang::as_quosure(parse(text = "1\n2\n3\n4"), new.env())
  solution <- rlang::as_quosure(parse(text = "1\n2\n3\n4"), new.env())
  expect_null(
    detect_mistakes(user, solution)
  )

  user <- rlang::as_quosure(parse(text = "1\n8\n3\n4", new.env()))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = quote(8), that = quote(2)),
         category = category_mistake(quote(2)))
  )

  user <- rlang::as_quosure(parse(text = "1\n8\n3\n4", new.env()))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = wrong_value(this = quote(8), that = quote(2)),
         category = category_mistake(quote(2)))
  )

  user <- rlang::as_quosure(parse(text = "1\n2\n3\n4\n5", new.env()))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = extra_answer(quote(5)),
         category = category_mistake(quote(5)))
  )

  user <- rlang::as_quosure(parse(text = "1\n2\n3", new.env()))
  expect_equal(
    detect_mistakes(user, solution),
    list(message = missing_answer(quote(3)),
         category = category_mistake(quote(3)))
  )

})
