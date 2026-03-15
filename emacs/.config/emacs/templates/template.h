/*
 * File: `(file-name-nondirectory (buffer-file-name))`
 * Author: `user-full-name` <`user-mail-address`>
 * Brief: ${1:brief}
 *
 * Copyright (C) `(format-time-string "%Y")` `user-full-name`
 *
 * Licensed under the GPL License version 3.0
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

/*
 * History:
 * ================================================================
 * `(format-time-string "%Y-%m-%d %H:%M")` `user-full-name` <`user-mail-address`> created.
 */

#ifndef ${2:__`(upcase (replace-regexp-in-string "[^A-Za-z0-9]" "_" (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))`_H__}
#define $2

$0

#endif /* $2 */
