// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

__mixin Operations {
  // Here Self which is both the input and output type is a type variable
  fn F[self: Self](x: Self) -> Self {
     return x;
  }
}

class Point {
  fn Origin() -> Point {
    return {.x = 0, .y = 0};
  }

  var x: i32;
  var y: i32;
  __mix Operations;
}

fn Main() -> i32 {
  var p1: Point = Point.Origin();
  var p2: Point = {.x = 42, .y = 1};
  // After accessing the mixin member F through a Point object, the
  // input and output type variables of F get substituted with
  // the Point class type
  var p3: Point = p1.F(p2);
  return p3.x - 42;
}

// CHECK:STDOUT: result: 0
