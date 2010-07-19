// ----------------------------------------------------------------------------
//
//   Title    : %[Title: %]
//   Filename : %b
//   Language : 'e'
//   Author   : %u
//
//   Description : %1
//
// ----------------------------------------------------------------------------
// Copyright (c) %Y by Cisco Systems, Inc
// This model is the confidential and proprietary property of Cisco and the
// possession or use of this file requires written permission from Cisco.
// ----------------------------------------------------------------------------

<'
%(
(let ((pkg (e-mode-abbrev-get-prefix)))
  (if (and pkg (not (string= pkg "no_prefix")))
      (insert "package " pkg ";")
    (delete-char 2)))
%)

%@

'>
