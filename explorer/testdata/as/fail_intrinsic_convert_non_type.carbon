// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  // CHECK:STDERR: COMPILATION ERROR: fail_intrinsic_convert_non_type.carbon:[[@LINE+1]]: type error in type expression: 'i32' is not implicitly convertible to 'type'
  __intrinsic_implicit_as_convert("example", 0);
  return 0;
}
