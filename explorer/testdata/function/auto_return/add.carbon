// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn add(x: i32, y: i32) -> auto { return x + y; }

fn Main() -> i32 {
  return add(1, 2) - 3;
}

// CHECK:STDOUT: result: 0
