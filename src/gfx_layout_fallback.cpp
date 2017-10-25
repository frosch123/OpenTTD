/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file gfx_layout_icu.cpp Handling of laying out text using ICU Paragraph Layout. */

#include "stdafx.h"
#include "gfx_layout.h"
#include "string_func.h"
#include "strings_func.h"
#include "debug.h"

#include "table/control_codes.h"

#include <unicode/ustring.h>
#include <layout/ParagraphLayout.h>

#include "safeguards.h"



/* static */ Font *Font::Create(FontSize size, TextColour colour)
{
	return new Font(size, colour);
}


/*** Paragraph layout ***/
/**
 * Class handling the splitting of a paragraph of text into lines and
 * visual runs.
 *
 * One constructs this class with the text that needs to be split into
 * lines. Then nextLine is called with the maximum width until NULL is
 * returned. Each nextLine call creates VisualRuns which contain the
 * length of text that are to be drawn with the same font. In other
 * words, the result of this class is a list of sub strings with their
 * font. The sub strings are then already fully laid out, and only
 * need actual drawing.
 *
 * The positions in a visual run are sequential pairs of X,Y of the
 * begin of each of the glyphs plus an extra pair to mark the end.
 *
 * @note This variant does not handle left-to-right properly. This
 *       is supported in the one ParagraphLayout coming from ICU.
 */
class FallbackParagraphLayout : public ParagraphLayouter {
public:
	/** Helper for GetLayouter, to get the right type. */
	typedef WChar CharType;
	/** Helper for GetLayouter, to get whether the layouter supports RTL. */
	static const bool SUPPORTS_RTL = false;

	/** Visual run contains data about the bit of text with the same font. */
	class FallbackVisualRun : public ParagraphLayouter::VisualRun {
		Font *font;       ///< The font used to layout these.
		GlyphID *glyphs;  ///< The glyphs we're drawing.
		float *positions; ///< The positions of the glyphs.
		int *glyph_to_char; ///< The char index of the glyphs.
		int glyph_count;  ///< The number of glyphs.

	public:
		FallbackVisualRun(Font *font, const WChar *chars, int glyph_count, int x);
		~FallbackVisualRun();
		const Font *GetFont() const;
		int GetGlyphCount() const;
		const GlyphID *GetGlyphs() const;
		const float *GetPositions() const;
		int GetLeading() const;
		const int *GetGlyphToCharMap() const;
	};

	/** A single line worth of VisualRuns. */
	class FallbackLine : public AutoDeleteSmallVector<FallbackVisualRun *, 4>, public ParagraphLayouter::Line {
	public:
		int GetLeading() const;
		int GetWidth() const;
		int CountRuns() const;
		const ParagraphLayouter::VisualRun *GetVisualRun(int run) const;

		int GetInternalCharLength(WChar c) const { return 1; }
	};

	const WChar *buffer_begin; ///< Begin of the buffer.
	const WChar *buffer;       ///< The current location in the buffer.
	FontMap &runs;             ///< The fonts we have to use for this paragraph.

	FallbackParagraphLayout(WChar *buffer, int length, FontMap &runs);
	void Reflow();
	const ParagraphLayouter::Line *NextLine(int max_width);
};

/**
 * Create the visual run.
 * @param font       The font to use for this run.
 * @param chars      The characters to use for this run.
 * @param char_count The number of characters in this run.
 * @param x          The initial x position for this run.
 */
FallbackParagraphLayout::FallbackVisualRun::FallbackVisualRun(Font *font, const WChar *chars, int char_count, int x) :
		font(font), glyph_count(char_count)
{
	this->glyphs = MallocT<GlyphID>(this->glyph_count);
	this->glyph_to_char = MallocT<int>(this->glyph_count);

	/* Positions contains the location of the begin of each of the glyphs, and the end of the last one. */
	this->positions = MallocT<float>(this->glyph_count * 2 + 2);
	this->positions[0] = x;
	this->positions[1] = 0;

	for (int i = 0; i < this->glyph_count; i++) {
		this->glyphs[i] = font->fc->MapCharToGlyph(chars[i]);
		this->positions[2 * i + 2] = this->positions[2 * i] + font->fc->GetGlyphWidth(this->glyphs[i]);
		this->positions[2 * i + 3] = 0;
		this->glyph_to_char[i] = i;
	}
}

/** Free all data. */
FallbackParagraphLayout::FallbackVisualRun::~FallbackVisualRun()
{
	free(this->positions);
	free(this->glyph_to_char);
	free(this->glyphs);
}

/**
 * Get the font associated with this run.
 * @return The font.
 */
const Font *FallbackParagraphLayout::FallbackVisualRun::GetFont() const
{
	return this->font;
}

/**
 * Get the number of glyphs in this run.
 * @return The number of glyphs.
 */
int FallbackParagraphLayout::FallbackVisualRun::GetGlyphCount() const
{
	return this->glyph_count;
}

/**
 * Get the glyphs of this run.
 * @return The glyphs.
 */
const GlyphID *FallbackParagraphLayout::FallbackVisualRun::GetGlyphs() const
{
	return this->glyphs;
}

/**
 * Get the positions of this run.
 * @return The positions.
 */
const float *FallbackParagraphLayout::FallbackVisualRun::GetPositions() const
{
	return this->positions;
}

/**
 * Get the glyph-to-character map for this visual run.
 * @return The glyph-to-character map.
 */
const int *FallbackParagraphLayout::FallbackVisualRun::GetGlyphToCharMap() const
{
	return this->glyph_to_char;
}

/**
 * Get the height of this font.
 * @return The height of the font.
 */
int FallbackParagraphLayout::FallbackVisualRun::GetLeading() const
{
	return this->GetFont()->fc->GetHeight();
}

/**
 * Get the height of the line.
 * @return The maximum height of the line.
 */
int FallbackParagraphLayout::FallbackLine::GetLeading() const
{
	int leading = 0;
	for (const FallbackVisualRun * const *run = this->Begin(); run != this->End(); run++) {
		leading = max(leading, (*run)->GetLeading());
	}

	return leading;
}

/**
 * Get the width of this line.
 * @return The width of the line.
 */
int FallbackParagraphLayout::FallbackLine::GetWidth() const
{
	if (this->Length() == 0) return 0;

	/*
	 * The last X position of a run contains is the end of that run.
	 * Since there is no left-to-right support, taking this value of
	 * the last run gives us the end of the line and thus the width.
	 */
	const ParagraphLayouter::VisualRun *run = this->GetVisualRun(this->CountRuns() - 1);
	return (int)run->GetPositions()[run->GetGlyphCount() * 2];
}

/**
 * Get the number of runs in this line.
 * @return The number of runs.
 */
int FallbackParagraphLayout::FallbackLine::CountRuns() const
{
	return this->Length();
}

/**
 * Get a specific visual run.
 * @return The visual run.
 */
const ParagraphLayouter::VisualRun *FallbackParagraphLayout::FallbackLine::GetVisualRun(int run) const
{
	return *this->Get(run);
}

/**
 * Create a new paragraph layouter.
 * @param buffer The characters of the paragraph.
 * @param length The length of the paragraph.
 * @param runs   The font mapping of this paragraph.
 */
FallbackParagraphLayout::FallbackParagraphLayout(WChar *buffer, int length, FontMap &runs) : buffer_begin(buffer), buffer(buffer), runs(runs)
{
	assert(runs.End()[-1].first == length);
}

/**
 * Reset the position to the start of the paragraph.
 */
void FallbackParagraphLayout::Reflow()
{
	this->buffer = this->buffer_begin;
}

/**
 * Construct a new line with a maximum width.
 * @param max_width The maximum width of the string.
 * @return A Line, or NULL when at the end of the paragraph.
 */
const ParagraphLayouter::Line *FallbackParagraphLayout::NextLine(int max_width)
{
	/* Simple idea:
	 *  - split a line at a newline character, or at a space where we can break a line.
	 *  - split for a visual run whenever a new line happens, or the font changes.
	 */
	if (this->buffer == NULL) return NULL;

	FallbackLine *l = new FallbackLine();

	if (*this->buffer == '\0') {
		/* Only a newline. */
		this->buffer = NULL;
		*l->Append() = new FallbackVisualRun(this->runs.Begin()->second, this->buffer, 0, 0);
		return l;
	}

	const WChar *begin = this->buffer;
	const WChar *last_space = NULL;
	const WChar *last_char = begin;
	int width = 0;

	int offset = this->buffer - this->buffer_begin;
	FontMap::iterator iter = this->runs.Begin();
	while (iter->first <= offset) {
		iter++;
		assert(iter != this->runs.End());
	}

	const FontCache *fc = iter->second->fc;
	const WChar *next_run = this->buffer_begin + iter->first;

	for (;;) {
		WChar c = *this->buffer;
		last_char = this->buffer;

		if (c == '\0') {
			this->buffer = NULL;
			break;
		}

		if (this->buffer == next_run) {
			int w = l->GetWidth();
			*l->Append() = new FallbackVisualRun(iter->second, begin, this->buffer - begin, w);
			iter++;
			assert(iter != this->runs.End());

			next_run = this->buffer_begin + iter->first;
			begin = this->buffer;

			last_space = NULL;
		}

		if (IsWhitespace(c)) last_space = this->buffer;

		if (IsPrintable(c) && !IsTextDirectionChar(c)) {
			int char_width = GetCharacterWidth(fc->GetSize(), c);
			width += char_width;
			if (width > max_width) {
				/* The string is longer than maximum width so we need to decide
				 * what to do with it. */
				if (width == char_width) {
					/* The character is wider than allowed width; don't know
					 * what to do with this case... bail out! */
					this->buffer = NULL;
					return l;
				}

				if (last_space == NULL) {
					/* No space has been found. Just terminate at our current
					 * location. This usually happens for languages that do not
					 * require spaces in strings, like Chinese, Japanese and
					 * Korean. For other languages terminating mid-word might
					 * not be the best, but terminating the whole string instead
					 * of continuing the word at the next line is worse. */
					last_char = this->buffer;
				} else {
					/* A space is found; perfect place to terminate */
					this->buffer = last_space + 1;
					last_char = last_space;
				}
				break;
			}
		}

		this->buffer++;
	}

	if (l->Length() == 0 || last_char - begin != 0) {
		int w = l->GetWidth();
		*l->Append() = new FallbackVisualRun(iter->second, begin, last_char - begin, w);
	}
	return l;
}

