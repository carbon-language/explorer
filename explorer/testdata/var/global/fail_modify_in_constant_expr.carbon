// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

var n: i32 = 0;

fn F() -> type {
  // TODO: This isn't a very good description of the problem, which is that
  // compile-time evaluation doesn't have a mutable `n` value available.
  // CHECK:STDERR: COMPILATION ERROR: fail_modify_in_constant_expr.carbon:[[@LINE+1]]: could not find `n: i32`
  n = 1;
  return i32;
}

fn Main() -> F() {
  return n;
}
