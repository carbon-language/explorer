// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  // CHECK:STDERR: SYNTAX ERROR: fail_unsupported_integer_type.carbon:[[@LINE+1]]: Only i32 is supported for now: i64
  var x: i64 = 1;
  return 0;
}
