// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

// CHECK:STDERR: COMPILATION ERROR: fail_impl_as_not_constraint.carbon:[[@LINE+1]]: expected a constraint in impl declaration, found i32
impl i32 as i32 {}

fn Main() -> i32 {
}
