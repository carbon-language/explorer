// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

class Point {
  var x: i32;
  var y: i32;
}

fn Main() -> i32 {
  // CHECK:STDERR: COMPILATION ERROR: fail_field_missing.carbon:[[@LINE+1]]: type error in initializer of variable: '{.x: i32}' is not implicitly convertible to 'class Point'
  var p: Point = {.x = 1};
  return p.x - 1;
}
