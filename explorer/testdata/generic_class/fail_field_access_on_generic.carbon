// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn BadSimpleMemberAccess[T:! type](a: T) -> T {
  // CHECK:STDERR: COMPILATION ERROR: fail_field_access_on_generic.carbon:[[@LINE+1]]: member access in unconstrained type
  return a.x;
}

fn Main() -> i32 {
  return BadSimpleMemberAccess(0);
}
