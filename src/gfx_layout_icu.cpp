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



/**
 * Container with information about a font.
 */
class ICUFont : public LEFontInstance, public Font {
public:
	ICUFont(FontSize size, TextColour colour) : Font(size, colour) {}

	/* Implementation details of LEFontInstance */

	le_int32 getUnitsPerEM() const;
	le_int32 getAscent() const;
	le_int32 getDescent() const;
	le_int32 getLeading() const;
	float getXPixelsPerEm() const;
	float getYPixelsPerEm() const;
	float getScaleFactorX() const;
	float getScaleFactorY() const;
	const void *getFontTable(LETag tableTag) const;
	const void *getFontTable(LETag tableTag, size_t &length) const;
	LEGlyphID mapCharToGlyph(LEUnicode32 ch) const;
	void getGlyphAdvance(LEGlyphID glyph, LEPoint &advance) const;
	le_bool getGlyphPoint(LEGlyphID glyph, le_int32 pointNumber, LEPoint &point) const;
};

/* static */ Font *Font::Create(FontSize size, TextColour colour)
{
	return new ICUFont(size, colour);
}

#ifdef WITH_ICU_LAYOUT
/* Implementation details of LEFontInstance */

le_int32 Font::getUnitsPerEM() const
{
	return this->fc->GetUnitsPerEM();
}

le_int32 Font::getAscent() const
{
	return this->fc->GetAscender();
}

le_int32 Font::getDescent() const
{
	return -this->fc->GetDescender();
}

le_int32 Font::getLeading() const
{
	return this->fc->GetHeight();
}

float Font::getXPixelsPerEm() const
{
	return (float)this->fc->GetHeight();
}

float Font::getYPixelsPerEm() const
{
	return (float)this->fc->GetHeight();
}

float Font::getScaleFactorX() const
{
	return 1.0f;
}

float Font::getScaleFactorY() const
{
	return 1.0f;
}

const void *Font::getFontTable(LETag tableTag) const
{
	size_t length;
	return this->getFontTable(tableTag, length);
}

const void *Font::getFontTable(LETag tableTag, size_t &length) const
{
	return this->fc->GetFontTable(tableTag, length);
}

LEGlyphID Font::mapCharToGlyph(LEUnicode32 ch) const
{
	if (IsTextDirectionChar(ch)) return 0;
	return this->fc->MapCharToGlyph(ch);
}

void Font::getGlyphAdvance(LEGlyphID glyph, LEPoint &advance) const
{
	advance.fX = glyph == 0xFFFF ? 0 : this->fc->GetGlyphWidth(glyph);
	advance.fY = 0;
}

le_bool Font::getGlyphPoint(LEGlyphID glyph, le_int32 pointNumber, LEPoint &point) const
{
	return FALSE;
}

static size_t AppendToBuffer(UChar *buff, const UChar *buffer_last, WChar c)
{
	/* Transform from UTF-32 to internal ICU format of UTF-16. */
	int32 length = 0;
	UErrorCode err = U_ZERO_ERROR;
	u_strFromUTF32(buff, buffer_last - buff, &length, (UChar32*)&c, 1, &err);
	return length;
}

/**
 * Wrapper for doing layouts with ICU.
 */
class ICUParagraphLayout : public AutoDeleteSmallVector<ParagraphLayouter::Line *, 4>, public ParagraphLayouter {
	ParagraphLayout *p; ///< The actual ICU paragraph layout.
public:
	/** Helper for GetLayouter, to get the right type. */
	typedef UChar CharType;
	/** Helper for GetLayouter, to get whether the layouter supports RTL. */
	static const bool SUPPORTS_RTL = true;

	/** Visual run contains data about the bit of text with the same font. */
	class ICUVisualRun : public ParagraphLayouter::VisualRun {
		const ParagraphLayout::VisualRun *vr; ///< The actual ICU vr.

	public:
		ICUVisualRun(const ParagraphLayout::VisualRun *vr) : vr(vr) { }

		const Font *GetFont() const          { return (const Font*)vr->getFont(); }
		int GetGlyphCount() const            { return vr->getGlyphCount(); }
		const GlyphID *GetGlyphs() const     { return vr->getGlyphs(); }
		const float *GetPositions() const    { return vr->getPositions(); }
		int GetLeading() const               { return vr->getLeading(); }
		const int *GetGlyphToCharMap() const { return vr->getGlyphToCharMap(); }
	};

	/** A single line worth of VisualRuns. */
	class ICULine : public AutoDeleteSmallVector<ICUVisualRun *, 4>, public ParagraphLayouter::Line {
		ParagraphLayout::Line *l; ///< The actual ICU line.

	public:
		ICULine(ParagraphLayout::Line *l) : l(l)
		{
			for (int i = 0; i < l->countRuns(); i++) {
				*this->Append() = new ICUVisualRun(l->getVisualRun(i));
			}
		}
		~ICULine() { delete l; }

		int GetLeading() const { return l->getLeading(); }
		int GetWidth() const   { return l->getWidth(); }
		int CountRuns() const  { return l->countRuns(); }
		const ParagraphLayouter::VisualRun *GetVisualRun(int run) const { return *this->Get(run); }

		int GetInternalCharLength(WChar c) const
		{
			/* ICU uses UTF-16 internally which means we need to account for surrogate pairs. */
			return Utf8CharLen(c) < 4 ? 1 : 2;
		}
	};

	ICUParagraphLayout(ParagraphLayout *p) : p(p) { }
	~ICUParagraphLayout() { delete p; }
	void Reflow() { p->reflow(); }

	ParagraphLayouter::Line *NextLine(int max_width)
	{
		ParagraphLayout::Line *l = p->nextLine(max_width);
		return l == NULL ? NULL : new ICULine(l);
	}
};

