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

fn GetX(p: Point) -> i32 {
  return p.x;
}

fn Main() -> i32 {
  return GetX({.x = 1, .y = 2}) - 1;
}

// CHECK:STDOUT: result: 0
