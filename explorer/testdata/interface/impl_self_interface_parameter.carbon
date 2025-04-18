// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

interface MyAddWith(T:! type) {
  fn Op[self: Self](b: T) -> Self;
}

class Point {
  var x: i32;
  var y: i32;
}

// Allowed: `Self` means `Point` after `as`
impl Point as MyAddWith(Self) {
  fn Op[self: Point](b: Point) -> Point {
    return {.x = self.x + b.x, .y = self.y + b.y};
  }
}

fn DoAddGeneric[T:! type, U:! MyAddWith(T)](a: U, b: T) -> U {
  return a.Op(b);
}

fn Main() -> i32 {
  var a: Point = {.x = 2, .y = 1};
  var b: Point = {.x = 4, .y = 2};
  var p: Point = DoAddGeneric(a, b);
  return p.x - p.y - 3;
}

// CHECK:STDOUT: result: 0
