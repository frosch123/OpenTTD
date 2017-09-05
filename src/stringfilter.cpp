/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file stringfilter.cpp Searching and filtering using a stringterm. */

#include "stdafx.h"
#include "string_func.h"
#include "strings_func.h"
#include "stringfilter_type.h"
#include "textbuf_type.h"
#include "gfx_func.h"

static const WChar STATE_WHITESPACE = ' ';
static const WChar STATE_WORD = 'w';
static const WChar STATE_QUOTE1 = '\'';
static const WChar STATE_QUOTE2 = '"';

/**
 * Set the term to filter on.
 * @param str Filter term
 */
void StringFilter::SetFilterTerm(const char *str)
{
	this->word_index.Reset();
	this->word_matches = 0;
	free(this->filter_buffer);

	assert(str != NULL);

	char *dest = (char *)malloc(strlen(str) + 1);
	this->filter_buffer = dest;

	WChar state = STATE_WHITESPACE;
	const char *pos = str;
	WordState *word = NULL;
	size_t len;
	for (;; pos += len) {
		WChar c;
		len = Utf8Decode(&c, pos);

		if (c == 0 || (state == STATE_WORD && IsWhitespace(c))) {
			/* Finish word */
			if (word != NULL) {
				*(dest++) = '\0';
				word = NULL;
			}
			state = STATE_WHITESPACE;
			if (c != 0) continue; else break;
		}

		if (state == STATE_WHITESPACE) {
			/* Skip whitespace */
			if (IsWhitespace(c)) continue;
			state = STATE_WORD;
		}

		if (c == STATE_QUOTE1 || c == STATE_QUOTE2) {
			if (state == c) {
				/* Stop quoting */
				state = STATE_WORD;
				continue;
			} else if (state == STATE_WORD) {
				/* Start quoting */
				state = c;
				continue;
			}
		}

		/* Add to word */
		if (word == NULL) {
			word = this->word_index.Append();
			word->start = dest;
			word->match = false;
		}

		memcpy(dest, pos, len);
		dest += len;
	}
}

/**
 * Reset the matching state to process a new item.
 */
void StringFilter::ResetState()
{
	this->word_matches = 0;
	const WordState *end = this->word_index.End();
	for (WordState *it = this->word_index.Begin(); it != end; ++it) {
		it->match = false;
	}
}

/**
 * Pass another text line from the current item to the filter.
 *
 * You can call this multiple times for a single item, if the filter shall apply to multiple things.
 * Before processing the next item you have to call ResetState().
 *
 * @param str Another line from the item.
 */
void StringFilter::AddLine(const char *str)
{
	if (str == NULL) return;

	bool match_case = this->case_sensitive != NULL && *this->case_sensitive;
	const WordState *end = this->word_index.End();
	for (WordState *it = this->word_index.Begin(); it != end; ++it) {
		if (!it->match) {
			if ((match_case ? strstr(str, it->start) : strcasestr(str, it->start)) != NULL) {
				it->match = true;
				this->word_matches++;
			}
		}
	}
}

/**
 * Pass another text line from the current item to the filter.
 *
 * You can call this multiple times for a single item, if the filter shall apply to multiple things.
 * Before processing the next item you have to call ResetState().
 *
 * @param str Another line from the item.
 */
void StringFilter::AddLine(StringID str)
{
	char buffer[DRAW_STRING_BUFFER];
	GetString(buffer, str, lastof(buffer));
	AddLine(buffer);
}

/**
 * Add or remove a word from the filter.
 * @param str Word to remove or add.
 */
void StringFilter::ToggleWord(const char *str)
{
	if (StrEmpty(str)) return;

	bool match_case = this->case_sensitive != NULL && *this->case_sensitive;
	bool present = false;
	const WordState *end = this->word_index.End();
	WordState *dest = this->word_index.Begin();
	size_t buf_size = 0;
	for (WordState *it = this->word_index.Begin(); it != end; ++it) {
		if ((match_case ? strcmp(str, it->start) : strcasecmp(str, it->start)) == 0) {
			present = true;
			if (it->match) this->word_matches--;
		} else {
			buf_size += strlen(it->start) + 1;
			if (it != dest) *dest = *it;
			dest++;
		}
	}
	this->word_index.Erase(dest, end - dest);

	if (!present) {
		/* Allocate new buffer, and copy everything over */
		buf_size += strlen(str) + 1;
		char *new_buf = (char*)malloc(buf_size);
		const char *last = new_buf + (buf_size - 1);

		char *pos = new_buf;
		const WordState *end = this->word_index.End();
		for (WordState *it = this->word_index.Begin(); it != end; ++it) {
			const char *src = it->start;
			it->start = pos;
			pos = strecpy(pos, src, last) + 1;
		}

		/* Add a word */
		WordState *word = this->word_index.Append();
		word->start = pos;
		word->match = false;
		strecpy(pos, str, last);

		free(this->filter_buffer);
		this->filter_buffer = new_buf;
	}
}

void StringFilter::GetFilterTerm(Textbuf *textbuf)
{
	char *buf_pos = textbuf->buf;
	char *buf_end = buf_pos + textbuf->max_bytes - 1;
	*buf_pos = '\0';

	const WordState *end = this->word_index.End();
	for (WordState *it = this->word_index.Begin(); it != end; ++it) {
		if (buf_pos >= buf_end) break;
		if (buf_pos != textbuf->buf) buf_pos = strecpy(buf_pos, " ", buf_end);

		WChar quote_state = STATE_WORD;
		uint has_quote1 = 0;
		uint has_quote2 = 0;
		bool has_space = false;
		for (const char *pos = it->start; *pos != '\0'; pos++) {
			if (*pos == '\'') {
				if (quote_state == STATE_WORD) quote_state = STATE_QUOTE2;
				has_quote1++;
			} else if (*pos == '"') {
				if (quote_state == STATE_WORD) quote_state = STATE_QUOTE1;
				has_quote2++;
			} else if (IsWhitespace(*pos)) {
				has_space = true;
			}
		}
		
		if (has_quote1 > 0 && has_quote2 > 0) {
			// TODO magic
		} else if (has_quote1 > 0 || has_quote2 > 0 || has_space) {
			/* Simple quoting */
			buf_pos = strecpy(buf_pos, has_quote2 > 0 ? "'" : "\"", buf_end);
			buf_pos = strecpy(buf_pos, it->start, buf_end);
			buf_pos = strecpy(buf_pos, has_quote2 > 0 ? "'" : "\"", buf_end);
		} else {
			/* No quoting */
			buf_pos = strecpy(buf_pos, it->start, buf_end);
		}
	}

	textbuf->UpdateSize();
}
