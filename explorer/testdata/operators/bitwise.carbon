// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  if (^0 != -1) { return 11; }
  if (^1 != -2) { return 12; }
  if (^(-3) != 2) { return 13; }

  if (3 & 6 != 2) { return 21; }
  if (-1 & 4 != 4) { return 22; }
  if (-1 & -2 != -2) { return 23; }

  if (1 | 4 != 5) { return 31; }
  if (5 | 3 != 7) { return 32; }
  if (-2 | 1 != -1) { return 33; }

  if (1 ^ 4 != 5) { return 41; }
  if (5 ^ 3 != 6) { return 42; }
  if (-2 ^ -3 != 3) { return 43; }

  return 0;
}

// CHECK:STDOUT: result: 0
