// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

interface IntLike {
  fn Convert[self: Self]() -> i32;
}
impl i32 as IntLike {
  fn Convert[self: i32]() -> i32 { return self; }
}

fn add[T:! IntLike](x: {.a: T, .b: ({.m: i32, .n: T}, i32)}) -> i32 {
  return 1000 * x.a.Convert() + 100 * x.b[0].m + 10 * x.b[0].n.Convert() + x.b[1];
}

fn Main() -> i32 {
  return add({.b = ({.n = 1, .m = 2}, 3), .a = 4});
}

// CHECK:STDOUT: result: 4213
