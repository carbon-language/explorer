// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

// TODO: Implement this with some kind of reflection?
impl {.x: i32, .y: i32} as EqWith(Self) {
  fn Equal[self: Self](other: Self) -> bool {
    return self.x == other.x and self.y == other.y;
  }
  fn NotEqual[self: Self](other: Self) -> bool {
    return self.x != other.x or self.y != other.y;
  }
}

fn Main() -> i32 {
  var t1: {.x: i32, .y: i32} = {.x = 5, .y = 2};
  var t2: {.x: i32, .y: i32} = {.x = 5, .y = 4};
  if (t1 == t2) {
    return 1;
  } else {
    return 0;
  }
}

// CHECK:STDOUT: result: 0
