;;; flycheck-drstring-test.el --- Tests for `flycheck-drstring'            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Daniel Martín

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'ert)
(require 'flycheck-drstring)

(ert-deftest flycheck-drstring--font-lock-error-explanation-test ()
  (let* ((explanation "========== DrString E011 ===========

Summary:
------------------------------------
Keywords such as ‘Parameters‘, ‘Throws‘, and ‘Returns‘ must be spelled
correctly. Their initial letter must be uppercased or lowercased depending on
_convention_ specified for the project.
------------------------------------

Bad example:
------------------------------------
/// Let’s say the convention is lower case
///
/// - Parameter bar: violation! Convention dictates ‘parameter‘
/// - parameterz baz: mis-spelled.
///
/// - return: spelling is wrong, missed an \"s\" at the end
func foo(bar: Int, baz: Int) -> Int
------------------------------------

Good example:
------------------------------------
/// Let’s say the convention is lower case
///
/// - parameter bar: good
/// - parameter baz: nice
///
/// - returns: got it
func foo(bar: Int, baz: Int) -> Int
------------------------------------

====================================")
         (font-lock-explanation
          (flycheck-drstring--font-lock-error-explanation explanation)))
    (should (equal (get-text-property 30 'face font-lock-explanation)
                   nil))
    (should (equal (get-text-property 630 'face font-lock-explanation)
                   nil))
    (should-not (equal (get-text-property 583 'face font-lock-explanation)
                       nil))
    (should-not (equal (get-text-property 833 'face font-lock-explanation)
                       nil))))

(ert-deftest flycheck-drstring--font-lock-error-wrong-explanation-test ()
  (let ((explanation "========== DrString E011 ===========

No examples here."))
    (should (equal-including-properties
             (flycheck-drstring--font-lock-error-explanation explanation)
             "========== DrString E011 ===========

No examples here."))))

(ert-deftest flycheck-drstring-parse-wrong-errors-test ()
  (let ((output "Invalid output.
"))
    (should (equal
             (flycheck-drstring-parse-errors output 'dummy-checker 'dummy-buffer)
             nil))))

(ert-deftest flycheck-drstring-parse-errors-test ()
  (let ((single-file "/sample.swift:2:0: warning: 1 docstring problem regarding foo(a:)
|E002| Missing docstring for a of type Int

Found 1 problem in 1 file in 0s 2ms
")
        (multiple-files "/string.swift:8:0: warning: 4 docstring problems regarding completelyDocumented(a:b:)
|E011| For a, Parameter is misspelled as parameter
|E011| For b, Parameter is misspelled as parameter
|E011| Throws is misspelled as throws
|E011| Returns is misspelled as returns

/string.swift:21:0: warning: 3 docstring problems regarding completelyDocumented2(a:b:)
|E011| Parameters is misspelled as parameters
|E011| Throws is misspelled as throws
|E011| Returns is misspelled as returns

Found 7 problems in 1 file in 0s 5ms
"))
    (should (equal
             (flycheck-drstring-parse-errors single-file 'dummy-checker 'dummy-buffer)
             '(#s(flycheck-error dummy-buffer dummy-checker "/sample.swift" 2 0 "Missing docstring for a of type Int" warning "E002" nil))))
    (should (equal
             (flycheck-drstring-parse-errors multiple-files 'dummy-checker 'dummy-buffer)
             '(#s(flycheck-error dummy-buffer dummy-checker "/string.swift" 8 0 "For a, Parameter is misspelled as parameter" warning "E011" nil)
                 #s(flycheck-error dummy-buffer dummy-checker "/string.swift" 8 0 "For b, Parameter is misspelled as parameter" warning "E011" nil)
                 #s(flycheck-error dummy-buffer dummy-checker "/string.swift" 8 0 "Throws is misspelled as throws" warning "E011" nil)
                 #s(flycheck-error dummy-buffer dummy-checker "/string.swift" 8 0 "Returns is misspelled as returns" warning "E011" nil)
                 #s(flycheck-error dummy-buffer dummy-checker "/string.swift" 21 0 "Parameters is misspelled as parameters" warning "E011" nil)
                 #s(flycheck-error dummy-buffer dummy-checker "/string.swift" 21 0 "Throws is misspelled as throws" warning "E011" nil)
                 #s(flycheck-error dummy-buffer dummy-checker "/string.swift" 21 0 "Returns is misspelled as returns" warning "E011" nil))))))
