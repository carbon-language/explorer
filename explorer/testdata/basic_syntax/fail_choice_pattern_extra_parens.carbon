// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

choice Ints {
  None,
  One(i32),
  Two(i32, i32)
}

fn Main() -> i32 {
  match (Ints.None) {
    // CHECK:STDERR: COMPILATION ERROR: fail_choice_pattern_extra_parens.carbon:[[@LINE+1]]: alternative `choice Ints.None` does not expect an argument list
    case Ints.None() => { return 0; }
    default => { return 1; }
  }
}
