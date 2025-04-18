// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

interface Number {
  fn Zero() -> Self;
  fn Add[self: Self](other: Self) -> Self;
}

class Point(T:! Number) {
  var x: T;
  var y: T;
}

impl i32 as Number {
  fn Zero() -> i32 { return 0; }
  fn Add[self: i32](other: i32) -> i32 { return self + other; }
}

impl forall [U:! Number] Point(U) as Number {
  fn Zero() -> Point(U) { return {.x = U.Zero(), .y = U.Zero() }; }
  fn Add[self: Point(U)](other: Point(U)) -> Point(U) {
    return {.x = self.x.Add(other.x), .y = self.y.Add(other.y)};
  }
}

fn Sum[E:! Number](x: E, y: E) -> E {
  var total: E = E.Zero();
  total = total.Add(x);
  total = total.Add(y);
  return total;
}

fn Main() -> i32 {
  var p: Point(i32) = {.x = 1, .y = 2};
  var q: Point(i32) = {.x = 4, .y = 3};
  var r: Point(i32) = Sum(p, q);
  return r.x - r.y;
}

// CHECK:STDOUT: result: 0
