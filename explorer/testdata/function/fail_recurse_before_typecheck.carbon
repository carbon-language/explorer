// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn T() -> type {
  // CHECK:STDERR: COMPILATION ERROR: fail_recurse_before_typecheck.carbon:[[@LINE+1]]: attempt to call function `T` that has not been fully type-checked
  var v: T() = 0;
  return i32;
}

fn Main() -> i32 {
  return 0;
}
