// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

class Point(T:! type) {
  var x: T;
  var y: T;
}

fn Main() -> i32 {
  var p: Point(i32) = {.x = 0, .y = 0};
  // CHECK:STDERR: COMPILATION ERROR: fail_point_equal.carbon:[[@LINE+1]]: type error in initializer of variable: 'class Point(T = i32)' is not implicitly convertible to 'class Point(T = bool)'
  var q: Point(bool) = p;
  return 0;
}
