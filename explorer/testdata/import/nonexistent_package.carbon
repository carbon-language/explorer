// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// TODO: This SHOULD fail but doesn't presently.
//
// NOAUTOUPDATE

package ExplorerTest api;

import ExplorerTest library "Nonexistent";

fn Main() -> i32 {
  return 0;
}

// CHECK:STDOUT: result: 0
