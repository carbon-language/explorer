// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest impl;

fn Main() -> i32 {
  // CHECK:STDERR: COMPILATION ERROR: fail_intrinsic_too_many_args.carbon:[[@LINE+1]]: Print takes 1 or 2 arguments, received 4
  __intrinsic_print("", 1, 2, 3);
}
