// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

// CHECK:STDERR: COMPILATION ERROR: fail_dot_self_not_in_scope.carbon:[[@LINE+1]]: could not resolve '.Self'
fn Main() -> .Self {
  return 0;
}
