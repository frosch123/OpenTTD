/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file newgrf_debug_gui.cpp GUIs for debugging NewGRFs. */

#include "stdafx.h"
#include <stdarg.h>
#include "window_gui.h"
#include "window_func.h"
#include "fileio_func.h"
#include "spritecache.h"
#include "string_func.h"
#include "strings_func.h"
#include "textbuf_gui.h"
#include "querystring_gui.h"
#include "vehicle_gui.h"

#include "engine_base.h"
#include "industry.h"
#include "object_base.h"
#include "station_base.h"
#include "town.h"
#include "vehicle_base.h"
#include "train.h"
#include "roadveh.h"

#include "newgrf_airporttiles.h"
#include "newgrf_debug.h"
#include "newgrf_object.h"
#include "newgrf_spritegroup.h"
#include "newgrf_station.h"
#include "newgrf_town.h"
#include "newgrf_railtype.h"
#include "newgrf_industries.h"
#include "newgrf_industrytiles.h"

#include "widgets/newgrf_debug_widget.h"

#include "table/strings.h"

/** The sprite picker. */
NewGrfDebugSpritePicker _newgrf_debug_sprite_picker = { SPM_NONE, NULL, 0, SmallVector<SpriteID, 256>() };

/**
 * Get the feature index related to the window number.
 * @param window_number The window to get the feature index from.
 * @return the feature index
 */
static inline uint GetFeatureIndex(uint window_number)
{
	return GB(window_number, 0, 24);
}

/**
 * Get the window number for the inspect window given a
 * feature and index.
 * @param feature The feature we want to inspect.
 * @param index   The index/identifier of the feature to inspect.
 * @return the InspectWindow (Window)Number
 */
static inline uint GetInspectWindowNumber(GrfSpecFeature feature, uint index)
{
	assert((index >> 24) == 0);
	return (feature << 24) | index;
}

/**
 * Parameters to stuff in the inspect window.
 */
enum NIParameter {
	NIP_BEGIN    = 0,

	NIP_VAR60    = NIP_BEGIN,           ///< Var 60+x parameter.
	NIP_VAR10,                          ///< Callback info 1.
	NIP_VAR18,                          ///< Callback info 2.
	NIP_REG100,                         ///< Temporary storage register 100 to 10F.
	NIP_REG101   = NIP_REG100 + 0x01,
	NIP_REG10E   = NIP_REG100 + 0x0E,
	NIP_REG10F   = NIP_REG100 + 0x0F,

	NIP_END      = NIP_REG100 + 0x10
};
DECLARE_POSTFIX_INCREMENT(NIParameter)
assert_compile(WID_NGRFI_PARAM_LABEL == WID_NGRFI_PARAM_SEL   + NIP_END);
assert_compile(WID_NGRFI_PARAM_EDIT  == WID_NGRFI_PARAM_LABEL + NIP_END);
assert_compile(WID_NGRFI_RESULT      == WID_NGRFI_PARAM_EDIT  + NIP_END);

/**
 * Bitset of NIParameter.
 */
enum NIParameterBits {
	NIP_BIT_NONE   = 0U,
	NIP_BIT_VAR60  = 1U << NIP_VAR60,   ///< Var 60+x paramter
	NIP_BIT_VAR10  = 1U << NIP_VAR10,   ///< Callback info 1.
	NIP_BIT_VAR18  = 1U << NIP_VAR18,   ///< Callback info 2.
	NIP_BIT_REG100 = 1U << NIP_REG100,  ///< Temporary storage register 100.
	NIP_BIT_REG101 = 1U << NIP_REG101,
	NIP_BIT_REG10E = 1U << NIP_REG10E,
	NIP_BIT_REG10F = 1U << NIP_REG10F,
};
DECLARE_ENUM_AS_BIT_SET(NIParameterBits)

static const char * const _newgrf_parameter_names[] = {
	"60+x parameter",
	"Variable 10",
	"Variable 18",
	"Register 100",
	"Register 101",
	"Register 102",
	"Register 103",
	"Register 104",
	"Register 105",
	"Register 106",
	"Register 107",
	"Register 108",
	"Register 109",
	"Register 10A",
	"Register 10B",
	"Register 10C",
	"Register 10D",
	"Register 10E",
	"Register 10F",
};
assert_compile(lengthof(_newgrf_parameter_names) == NIP_END);

/**
 * Return values of callbacks.
 */
enum NICBResult {
	NICBR_BEGIN,

	NICBR_RESULT   = NICBR_BEGIN,         ///< Normal callback result.
	NICBR_REG100,                         ///< Temporary storage register 100 to 10F.

	NICBR_END      = NICBR_REG100 + 0x10
};

DECLARE_POSTFIX_INCREMENT(NICBResult)

/**
 * Bitset of NICBResult.
 */
enum NICBResultBits {
	NICBR_BIT_NONE       = 0U,
	NICBR_BIT_RESULT     = 1U << NICBR_RESULT,    ///< Normal callback result.
	NICBR_BIT_REG100     = 1U << NICBR_REG100,    ///< Temporary storage register 100.
	NICBR_BIT_TEXTSTACK4 = 0x0FU << NICBR_REG100, ///< 4 register textstack 100..103
	NICBR_BIT_TEXTSTACK6 = 0x3FU << NICBR_REG100, ///< 6 register textstack 100..105
};
DECLARE_ENUM_AS_BIT_SET(NICBResultBits)

static const char * const _newgrf_cbresult_names[] = {
	"Result",
	"Register 100",
	"Register 101",
	"Register 102",
	"Register 103",
	"Register 104",
	"Register 105",
	"Register 106",
	"Register 107",
	"Register 108",
	"Register 109",
	"Register 10A",
	"Register 10B",
	"Register 10C",
	"Register 10D",
	"Register 10E",
	"Register 10F",
};
assert_compile(lengthof(_newgrf_cbresult_names) == NICBR_END);

/**
 * The type of a property to show. This is used to
 * provide an appropriate representation in the GUI.
 */
enum NIType {
	NIT_INT,   ///< The property is a simple integer
	NIT_CARGO, ///< The property is a cargo
};

/** Representation of the data from a NewGRF property. */
struct NIProperty {
	const char *name;       ///< A (human readable) name for the property
	ptrdiff_t offset;       ///< Offset of the variable in the class
	byte read_size;         ///< Number of bytes (i.e. byte, word, dword etc)
	byte prop;              ///< The number of the property
	byte type;
};


/**
 * Representation of the available callbacks with
 * information on when they actually apply.
 */
struct NICallback {
	const char *name; ///< The human readable name of the callback
	ptrdiff_t offset; ///< Offset of the variable in the class
	byte read_size;   ///< The number of bytes (i.e. byte, word, dword etc) to read
	byte cb_bit;      ///< The bit that needs to be set for this callback to be enabled
	uint16 cb_id;     ///< The number of the callback
	NIParameterBits params; ///< Parameters to the callback
	NICBResultBits results; ///< Results from the callback
};
/** Mask to show no bit needs to be enabled for the callback. */
static const int CBM_NO_BIT = UINT8_MAX;

/** Representation on the NewGRF variables. */
struct NIVariable {
	const char *name;
	byte var;
	NIParameterBits params;
};

/**
 * Parameter values to NewGRF inspect stuff.
 * This is basically a 'typedef uint32 NIParameters[NIP_END]', but with default constructor, so it is suitable for SDL containers.
 */
struct NIParameters {
	uint32 param[NIP_END];

	uint32 &operator[](uint index) { return param[index]; }
	const uint32 &operator[](uint index) const { return param[index]; }
};

/** Result values of callbacks */
typedef uint32 NICBResults[NICBR_END];

/** Helper class to wrap some functionality/queries in. */
class NIHelper {
public:
	/** Silence a warning. */
	virtual ~NIHelper() {}

	/**
	 * Is the item with the given index inspectable?
	 * @param index the index to check.
	 * @return true iff the index is inspectable.
	 */
	virtual bool IsInspectable(uint index) const = 0;

	/**
	 * Get the parent "window_number" of a given instance.
	 * @param index the instance to get the parent for.
	 * @return the parent's window_number or UINT32_MAX if there is none.
	 */
	virtual uint GetParent(uint index) const = 0;

	/**
	 * Get the instance given an index.
	 * @param index the index to get the instance for.
	 * @return the instance.
	 */
	virtual const void *GetInstance(uint index) const = 0;

	/**
	 * Get (NewGRF) specs given an index.
	 * @param index the index to get the specs for for.
	 * @return the specs.
	 */
	virtual const void *GetSpec(uint index) const = 0;

	/**
	 * Set the string parameters to write the right data for a STRINGn.
	 * @param index the index to get the string parameters for.
	 */
	virtual void SetStringParameters(uint index) const = 0;

	/**
	 * Get the GRFID of the file that includes this item.
	 * @param index index to check.
	 * @return GRFID of the item. 0 means that the item is not inspectable.
	 */
	virtual uint32 GetGRFID(uint index) const = 0;

	/**
	 * Resolve (action2) variable for a given scope.
	 * @param scope Scope of the variable.
	 * @param var   The variable to actually resolve.
	 * @param param Additional parameters to pass.
	 * @param avail Return whether the variable is available.
	 * @return The resolved variable's value.
	 */
	uint Resolve(ScopeResolver *scope, uint var, const NIParameters &param, bool *avail) const
	{
		for (uint i = 0; i <= 0x0F; i++) {
			SetRegister(0x100 + i, param[NIP_REG100 + i]);
		}
		return scope->GetVariable(var, param[NIP_VAR60], avail);
	}

	/**
	 * Resolve (action2) variable for a given index.
	 * @param index The (instance) index to resolve the variable for.
	 * @param var   The variable to actually resolve.
	 * @param param Additional parameters to pass.
	 * @param avail Return whether the variable is available.
	 * @return The resolved variable's value.
	 */
	virtual uint Resolve(uint index, uint var, const NIParameters &param, bool *avail) const = 0;

	/**
	 * Resolve callback
	 * @param ro ResolverObject to resolve callback for
	 * @param cb Callback to call
	 * @param param Addtional paramters
	 * @param [out] result Result values
	 */
	void ResolveCB(ResolverObject &ro, CallbackID cb, const NIParameters &param, NICBResults &result) const
	{
		ro.callback = cb;
		ro.callback_param1 = param[NIP_VAR10];
		ro.callback_param2 = param[NIP_VAR18];
		result[NICBR_RESULT] = ro.ResolveCallback();
		for (uint i = 0; i <= 0x0F; i++) {
			result[NICBR_REG100 + i] = GetRegister(0x100 + i);
		}
	}

	/**
	 * Resolve callback for a given index.
	 * @param index The (instance) index to resolve the variable for.
	 * @param cb    The callback ID to actually resolve.
	 * @param param Additional parameters to pass.
	 * @param [out] result Callback result and return values.
	 */
	virtual void ResolveCB(uint index, CallbackID cb, const NIParameters &param, NICBResults &result) const = 0;

	/**
	 * Used to decide if the PSA needs a parameter or not.
	 * @return True iff this item has a PSA that requires a parameter.
	 */
	virtual bool PSAWithParameter() const
	{
		return false;
	}

	/**
	 * Allows to know the size of the persistent storage.
	 * @param index Index of the item.
	 * @param grfid Parameter for the PSA. Only required for items with parameters.
	 * @return Size of the persistent storage in indices.
	 */
	virtual uint GetPSASize(uint index, uint32 grfid) const
	{
		return 0;
	}

	/**
	 * Gets the first position of the array containing the persistent storage.
	 * @param index Index of the item.
	 * @param grfid Parameter for the PSA. Only required for items with parameters.
	 * @return Pointer to the first position of the storage array or NULL if not present.
	 */
	virtual const int32 *GetPSAFirstPosition(uint index, uint32 grfid) const
	{
		return NULL;
	}

protected:
	/**
	 * Helper to make setting the strings easier.
	 * @param string the string to actually draw.
	 * @param index  the (instance) index for the string.
	 */
	void SetSimpleStringParameters(StringID string, uint32 index) const
	{
		SetDParam(0, string);
		SetDParam(1, index);
	}


	/**
	 * Helper to make setting the strings easier for objects at a specific tile.
	 * @param string the string to draw the object's name
	 * @param index  the (instance) index for the string.
	 * @param tile   the tile the object is at
	 */
	void SetObjectAtStringParameters(StringID string, uint32 index, TileIndex tile) const
	{
		SetDParam(0, STR_NEWGRF_INSPECT_CAPTION_OBJECT_AT);
		SetDParam(1, string);
		SetDParam(2, index);
		SetDParam(3, tile);
	}
};


/** Container for all information for a given feature. */
struct NIFeature {
	const NIProperty *properties; ///< The properties associated with this feature.
	const NICallback *callbacks;  ///< The callbacks associated with this feature.
	const NIVariable *variables;  ///< The variables associated with this feature.
	const NIHelper   *helper;     ///< The class container all helper functions.
};

/* Load all the NewGRF debug data; externalised as it is just a huge bunch of tables. */
#include "table/newgrf_debug_data.h"

/**
 * Get the feature number related to the window number.
 * @param window_number The window to get the feature number for.
 * @return The feature number.
 */
static inline GrfSpecFeature GetFeatureNum(uint window_number)
{
	return (GrfSpecFeature)GB(window_number, 24, 8);
}

/**
 * Get the NIFeature related to the window number.
 * @param window_number The window to get the NIFeature for.
 * @return the NIFeature, or NULL is there isn't one.
 */
static inline const NIFeature *GetFeature(uint window_number)
{
	GrfSpecFeature idx = GetFeatureNum(window_number);
	return idx < GSF_FAKE_END ? _nifeatures[idx] : NULL;
}

/**
 * Get the NIHelper related to the window number.
 * @param window_number The window to get the NIHelper for.
 * @pre GetFeature(window_number) != NULL
 * @return the NIHelper
 */
static inline const NIHelper *GetFeatureHelper(uint window_number)
{
	return GetFeature(window_number)->helper;
}

/** Window used for inspecting NewGRFs. */
struct NewGRFInspectWindow : Window {
	static const int LEFT_OFFSET   = 5; ///< Position of left edge
	static const int RIGHT_OFFSET  = 5; ///< Position of right edge
	static const int TOP_OFFSET    = 5; ///< Position of top edge
	static const int BOTTOM_OFFSET = 5; ///< Position of bottom edge

	/** Identifies a row */
	struct RowKey {
		uint type;
		uint id;

		RowKey() : type(0), id(0) {}

		RowKey(const NIVariable *niv) : type(1), id(niv->var) {}
		RowKey(const NICallback *nic) : type(2), id(nic->cb_id) {}

		bool operator==(const RowKey &other) const
		{
			return this->type == other.type && this->id == other.id;
		}

		bool operator<(const RowKey &other) const
		{
			if (this->type != other.type) return this->type < other.type;
			return this->id < other.id;
		}
	};

	/** The parameters for the variables. */
	static std::map<RowKey, NIParameters> varparams[GSF_FAKE_END];

	/** GRFID of the caller of this window, 0 if it has no caller. */
	uint32 caller_grfid;

	/** For ground vehicles: Index in vehicle chain. */
	uint chain_index;

	/** The currently edited parameter, to update the right one. */
	RowKey current_edit_param;
	const NICallback *current_edit_callback;

	Scrollbar *vscroll;
	QueryString *editboxes[NIP_END];   ///< Editboxes for parameters

	/**
	 * Set the GRFID of the item opening this window.
	 * @param grfid GRFID of the item opening this window, or 0 if not opened by other window.
	 */
	void SetCallerGRFID(uint32 grfid)
	{
		this->caller_grfid = grfid;
		this->SetDirty();
	}

	/**
	 * Check whether this feature has chain index, i.e. refers to ground vehicles.
	 */
	bool HasChainIndex() const
	{
		GrfSpecFeature f = GetFeatureNum(this->window_number);
		return f == GSF_TRAINS || f == GSF_ROADVEHICLES;
	}

	/**
	 * Get the feature index.
	 * @return the feature index
	 */
	uint GetFeatureIndex() const
	{
		uint index = ::GetFeatureIndex(this->window_number);
		if (this->chain_index > 0) {
			assert(this->HasChainIndex());
			const Vehicle *v = Vehicle::Get(index);
			v = v->Move(this->chain_index);
			if (v != NULL) index = v->index;
		}
		return index;
	}

	/**
	 * Ensure that this->chain_index is in range.
	 */
	void ValidateChainIndex()
	{
		if (this->chain_index == 0) return;

		assert(this->HasChainIndex());

		const Vehicle *v = Vehicle::Get(::GetFeatureIndex(this->window_number));
		v = v->Move(this->chain_index);
		if (v == NULL) this->chain_index = 0;
	}

	NewGRFInspectWindow(WindowDesc *desc, WindowNumber wno) : Window(desc)
	{
		this->CreateNestedTree();
		this->vscroll = this->GetScrollbar(WID_NGRFI_SCROLLBAR);
		this->vscroll->SetStepSize(FONT_HEIGHT_NORMAL);
		for (NIParameter i = NIP_BEGIN; i != NIP_END; i++) {
			this->querystrings[WID_NGRFI_PARAM_EDIT + i] = this->editboxes[i] = new QueryString(9);
			this->editboxes[i]->text.afilter = CS_HEXADECIMAL;
			this->DisableWidget(WID_NGRFI_PARAM_EDIT + i);
			this->GetWidget<NWidgetStacked>(WID_NGRFI_PARAM_SEL + i)->SetDisplayedPlane(SZSP_HORIZONTAL);
		}
		this->FinishInitNested(wno);

		this->vscroll->SetCount(0);
		this->SetWidgetDisabledState(WID_NGRFI_PARENT, GetFeatureHelper(this->window_number)->GetParent(this->GetFeatureIndex()) == UINT32_MAX);

		this->OnInvalidateData(0, true);
	}

	~NewGRFInspectWindow()
	{
		for (NIParameter i = NIP_BEGIN; i != NIP_END; i++) {
			delete this->editboxes[i];
		}
	}

	virtual void SetStringParameters(int widget) const
	{
		if (IsInsideBS(widget, WID_NGRFI_PARAM_LABEL, NIP_END)) {
			SetDParamStr(0, _newgrf_parameter_names[widget - WID_NGRFI_PARAM_LABEL]);
			return;
		}

		if (widget != WID_NGRFI_CAPTION) return;

		GetFeatureHelper(this->window_number)->SetStringParameters(this->GetFeatureIndex());
	}

	virtual void UpdateWidgetSize(int widget, Dimension *size, const Dimension &padding, Dimension *fill, Dimension *resize)
	{
		switch (widget) {
			case WID_NGRFI_VEH_CHAIN: {
				assert(this->HasChainIndex());
				GrfSpecFeature f = GetFeatureNum(this->window_number);
				size->height = max(size->height, GetVehicleImageCellSize((VehicleType)(VEH_TRAIN + (f - GSF_TRAINS)), EIT_IN_DEPOT).height + 2 + WD_BEVEL_TOP + WD_BEVEL_BOTTOM);
				break;
			}

			case WID_NGRFI_MAINPANEL:
				resize->height = 1;
				resize->width  = 1;

				size->height = 5 * FONT_HEIGHT_NORMAL + TOP_OFFSET + BOTTOM_OFFSET;
				break;

			case WID_NGRFI_RESULT: {
				uint rows = this->current_edit_callback != NULL ? CountBits(this->current_edit_callback->results) : 0;
				size->height = rows * FONT_HEIGHT_NORMAL + WD_FRAMERECT_TOP + WD_FRAMERECT_BOTTOM;
				break;
			}
		}
	}

	/**
	 * Helper function to draw a string (line) in the window.
	 * @param r      The (screen) rectangle we must draw within
	 * @param offset The offset (in lines) we want to draw for
	 * @param selected Whether the line is selected
	 * @param format The format string
	 */
	void WARN_FORMAT(5, 6) DrawString(const Rect &r, int offset, bool selected, const char *format, ...) const
	{
		char buf[1024];

		va_list va;
		va_start(va, format);
		vsnprintf(buf, lengthof(buf), format, va);
		va_end(va);

		offset *= FONT_HEIGHT_NORMAL;
		offset -= this->vscroll->GetPosition();
		if (offset < 0 || offset >= this->vscroll->GetCapacity()) return;

		::DrawString(r.left + LEFT_OFFSET, r.right - RIGHT_OFFSET, r.top + TOP_OFFSET + offset, buf, selected ? TC_WHITE : TC_BLACK);
	}

	virtual void DrawWidget(const Rect &r, int widget) const
	{
		switch (widget) {
			case WID_NGRFI_VEH_CHAIN: {
				const Vehicle *v = Vehicle::Get(this->GetFeatureIndex());
				int total_width = 0;
				int sel_start = 0;
				int sel_end = 0;
				for (const Vehicle *u = v->First(); u != NULL; u = u->Next()) {
					if (u == v) sel_start = total_width;
					switch (u->type) {
						case VEH_TRAIN: total_width += Train      ::From(u)->GetDisplayImageWidth(); break;
						case VEH_ROAD:  total_width += RoadVehicle::From(u)->GetDisplayImageWidth(); break;
						default: NOT_REACHED();
					}
					if (u == v) sel_end = total_width;
				}

				int width = r.right + 1 - r.left - WD_BEVEL_LEFT - WD_BEVEL_RIGHT;
				int skip = 0;
				if (total_width > width) {
					int sel_center = (sel_start + sel_end) / 2;
					if (sel_center > width / 2) skip = min(total_width - width, sel_center - width / 2);
				}

				GrfSpecFeature f = GetFeatureNum(this->window_number);
				int h = GetVehicleImageCellSize((VehicleType)(VEH_TRAIN + (f - GSF_TRAINS)), EIT_IN_DEPOT).height;
				int y = (r.top + r.bottom - h) / 2;
				DrawVehicleImage(v->First(), r.left + WD_BEVEL_LEFT, r.right - WD_BEVEL_RIGHT, y + 1, INVALID_VEHICLE, EIT_IN_DETAILS, skip);

				/* Highlight the articulated part (this is different to the whole-vehicle highlighting of DrawVehicleImage */
				if (_current_text_dir == TD_RTL) {
					DrawFrameRect(r.right - sel_end   + skip, y, r.right - sel_start + skip, y + h, COLOUR_WHITE, FR_BORDERONLY);
				} else {
					DrawFrameRect(r.left  + sel_start - skip, y, r.left  + sel_end   - skip, y + h, COLOUR_WHITE, FR_BORDERONLY);
				}
				break;
			}

			case WID_NGRFI_RESULT:
				if (this->current_edit_callback != NULL) {
					uint index = this->GetFeatureIndex();
					const NIFeature *nif = GetFeature(this->window_number);
					const NIHelper *nih = nif->helper;
					NIParameters &param = NewGRFInspectWindow::varparams[GetFeatureNum(this->window_number)][this->current_edit_param];
					NICBResults results;
					nih->ResolveCB(index, (CallbackID)this->current_edit_callback->cb_id, param, results);
					uint row = 0;
					for (NICBResult i = NICBR_BEGIN; i != NICBR_END; i++) {
						if (!HasBit(this->current_edit_callback->results, i)) continue;
						char valuestr[32];
						if (i == NICBR_RESULT && results[i] == CALLBACK_FAILED) {
							seprintf(valuestr, lastof(valuestr), "%s: failed", _newgrf_cbresult_names[i]);
						} else {
							seprintf(valuestr, lastof(valuestr), "%s: %08x", _newgrf_cbresult_names[i], results[i]);
						}
						::DrawString(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, r.top + WD_FRAMERECT_TOP + row * FONT_HEIGHT_NORMAL, valuestr, TC_BLACK);
						row++;
					}
				}
				break;
		}

		if (widget != WID_NGRFI_MAINPANEL) return;

		uint index = this->GetFeatureIndex();
		const NIFeature *nif  = GetFeature(this->window_number);
		const NIHelper *nih   = nif->helper;
		const void *base      = nih->GetInstance(index);
		const void *base_spec = nih->GetSpec(index);

		uint i = 0;
		if (nif->variables != NULL) {
			this->DrawString(r, i++, false, "Variables:");
			for (const NIVariable *niv = nif->variables; niv->name != NULL; niv++) {
				bool avail = true;
				NIParameters &param = NewGRFInspectWindow::varparams[GetFeatureNum(this->window_number)][niv];
				uint value = nih->Resolve(index, niv->var, param, &avail);

				char valuestr[32];
				if (avail) {
					seprintf(valuestr, lastof(valuestr), "%08x", value);
				} else {
					seprintf(valuestr, lastof(valuestr), "not available");
				}

				if (niv->params != NIP_BIT_NONE) {
					char paramstr[128];
					char *pos = paramstr;
					*pos = '\0';
					for (NIParameter p = NIP_BEGIN; p != NIP_END; p++) {
						if (!HasBit(niv->params, p)) continue;
						if (pos != paramstr) pos += seprintf(pos, lastof(paramstr), ", ");
						pos += seprintf(pos, lastof(paramstr), "%08x", param[p]);
					}
					this->DrawString(r, i++, this->current_edit_param == niv, "  %02x[%s]: %s (%s)", niv->var, paramstr, valuestr, niv->name);
				} else {
					this->DrawString(r, i++, this->current_edit_param == niv, "  %02x: %s (%s)", niv->var, valuestr, niv->name);
				}
			}
		}

		if (nif->callbacks != NULL) {
			this->DrawString(r, i++, false, "Callbacks:");
			for (const NICallback *nic = nif->callbacks; nic->name != NULL; nic++) {
				NIParameters &param = NewGRFInspectWindow::varparams[GetFeatureNum(this->window_number)][nic];

				char paramstr[128];
				char *pos = paramstr;
				*pos = '\0';
				for (NIParameter p = NIP_BEGIN; p != NIP_END; p++) {
					if (!HasBit(nic->params, p)) continue;
					if (pos != paramstr) {
						pos += seprintf(pos, lastof(paramstr), ", ");
					} else {
						pos += seprintf(pos, lastof(paramstr), "[");
					}
					pos += seprintf(pos, lastof(paramstr), "%08x", param[p]);
				}
				if (pos != paramstr) seprintf(pos, lastof(paramstr), "]");

				NICBResults results;
				nih->ResolveCB(index, (CallbackID)nic->cb_id, param, results);
				char valuestr[32];
				if (!HasBit(nic->results, NICBR_RESULT)) {
					valuestr[0] = '\0';
				} else if (results[NICBR_RESULT] != CALLBACK_FAILED) {
					seprintf(valuestr, lastof(valuestr), "%08x", results[NICBR_RESULT]);
				} else {
					seprintf(valuestr, lastof(valuestr), "failed");
				}

				if (nic->cb_bit != CBM_NO_BIT) {
					const void *ptr = (const byte *)base_spec + nic->offset;
					uint value;
					switch (nic->read_size) {
						case 1: value = *(const uint8  *)ptr; break;
						case 2: value = *(const uint16 *)ptr; break;
						case 4: value = *(const uint32 *)ptr; break;
						default: NOT_REACHED();
					}

					if (HasBit(value, nic->cb_bit)) {
						this->DrawString(r, i++, this->current_edit_param == nic, "  %03x%s: %s (%s)", nic->cb_id, paramstr, valuestr, nic->name);
					} else {
						this->DrawString(r, i++, this->current_edit_param == nic, "  %03x: masked (%s)", nic->cb_id, nic->name);
					}
				} else {
					this->DrawString(r, i++, this->current_edit_param == nic, "  %03x%s: %s (%s, unmasked)", nic->cb_id, paramstr, valuestr, nic->name);
				}
			}
		}

		uint psa_size = nih->GetPSASize(index, this->caller_grfid);
		const int32 *psa = nih->GetPSAFirstPosition(index, this->caller_grfid);
		if (psa_size != 0 && psa != NULL) {
			if (nih->PSAWithParameter()) {
				this->DrawString(r, i++, false, "Persistent storage [%08X]:", BSWAP32(this->caller_grfid));
			} else {
				this->DrawString(r, i++, false, "Persistent storage:");
			}
			assert(psa_size % 4 == 0);
			for (uint j = 0; j < psa_size; j += 4, psa += 4) {
				this->DrawString(r, i++, false, "  %i: %i %i %i %i", j, psa[0], psa[1], psa[2], psa[3]);
			}
		}

		if (nif->properties != NULL) {
			this->DrawString(r, i++, false, "Properties:");
			for (const NIProperty *nip = nif->properties; nip->name != NULL; nip++) {
				const void *ptr = (const byte *)base + nip->offset;
				uint value;
				switch (nip->read_size) {
					case 1: value = *(const uint8  *)ptr; break;
					case 2: value = *(const uint16 *)ptr; break;
					case 4: value = *(const uint32 *)ptr; break;
					default: NOT_REACHED();
				}

				StringID string;
				SetDParam(0, value);
				switch (nip->type) {
					case NIT_INT:
						string = STR_JUST_INT;
						break;

					case NIT_CARGO:
						string = value != INVALID_CARGO ? CargoSpec::Get(value)->name : STR_QUANTITY_N_A;
						break;

					default:
						NOT_REACHED();
				}

				char buffer[64];
				GetString(buffer, string, lastof(buffer));
				this->DrawString(r, i++, false, "  %02x: %s (%s)", nip->prop, buffer, nip->name);
			}
		}

		/* Not nice and certainly a hack, but it beats duplicating
		 * this whole function just to count the actual number of
		 * elements. Especially because they need to be redrawn. */
		const_cast<NewGRFInspectWindow*>(this)->vscroll->SetCount(i * FONT_HEIGHT_NORMAL);
	}

	virtual void OnClick(Point pt, int widget, int click_count)
	{
		switch (widget) {
			case WID_NGRFI_PARENT: {
				const NIHelper *nih   = GetFeatureHelper(this->window_number);
				uint index = nih->GetParent(this->GetFeatureIndex());
				::ShowNewGRFInspectWindow(GetFeatureNum(index), ::GetFeatureIndex(index), nih->GetGRFID(this->GetFeatureIndex()));
				break;
			}

			case WID_NGRFI_VEH_PREV:
				if (this->chain_index > 0) {
					this->chain_index--;
					this->InvalidateData();
				}
				break;

			case WID_NGRFI_VEH_NEXT:
				if (this->HasChainIndex()) {
					uint index = this->GetFeatureIndex();
					Vehicle *v = Vehicle::Get(index);
					if (v != NULL && v->Next() != NULL) {
						this->chain_index++;
						this->InvalidateData();
					}
				}
				break;

			case WID_NGRFI_MAINPANEL: {
				/* Does this feature have variables? */
				const NIFeature *nif  = GetFeature(this->window_number);
				if (nif->variables == NULL) return;

				/* Get the line, make sure it's within the boundaries. */
				int line = this->vscroll->GetScrolledRowFromWidget(pt.y, this, WID_NGRFI_MAINPANEL, TOP_OFFSET);
				if (line == INT_MAX) return;
				line /= FONT_HEIGHT_NORMAL;

				/* Find the variable related to the line */
				for (const NIVariable *niv = nif->variables; niv->name != NULL; niv++, line--) {
					if (line != 1) continue; // 1 because of the "Variables:" line

					this->current_edit_param = niv;
					this->current_edit_callback = NULL;
					NIParameters &param = NewGRFInspectWindow::varparams[GetFeatureNum(this->window_number)][niv];
					for (NIParameter i = NIP_BEGIN; i != NIP_END; i++) {
						bool active = HasBit(niv->params, i);
						this->SetWidgetDisabledState(WID_NGRFI_PARAM_EDIT + i, !active);
						this->GetWidget<NWidgetStacked>(WID_NGRFI_PARAM_SEL + i)->SetDisplayedPlane(active ? 0 : SZSP_HORIZONTAL);
						if (active) {
							char buf[16];
							snprintf(buf, sizeof(buf), "%08x", param[i]);
							this->editboxes[i]->text.Assign(buf);
						}
					}
					break;
				}

				for (const NICallback *nic = nif->callbacks; nic->name != NULL; nic++, line--) {
					if (line != 2) continue; // 2 because of the "Variables:" and "Callbacks:" line

					this->current_edit_param = nic;
					this->current_edit_callback = nic;
					NIParameters &param = NewGRFInspectWindow::varparams[GetFeatureNum(this->window_number)][nic];
					for (NIParameter i = NIP_BEGIN; i != NIP_END; i++) {
						bool active = HasBit(nic->params, i);
						this->SetWidgetDisabledState(WID_NGRFI_PARAM_EDIT + i, !active);
						this->GetWidget<NWidgetStacked>(WID_NGRFI_PARAM_SEL + i)->SetDisplayedPlane(active ? 0 : SZSP_HORIZONTAL);
						if (active) {
							char buf[16];
							snprintf(buf, sizeof(buf), "%08x", param[i]);
							this->editboxes[i]->text.Assign(buf);
						}
					}
					break;
				}

				this->ReInit();
				break;
			}
		}
	}

	virtual void OnEditboxChanged(int widget)
	{
		if (!IsInsideBS(widget, WID_NGRFI_PARAM_EDIT, NIP_END)) return;

		const char *str = this->editboxes[widget - WID_NGRFI_PARAM_EDIT]->text.buf;
		NewGRFInspectWindow::varparams[GetFeatureNum(this->window_number)][this->current_edit_param][widget - WID_NGRFI_PARAM_EDIT] = strtol(str, NULL, 16);
		this->SetDirty();
	}

	virtual void OnResize()
	{
		this->vscroll->SetCapacityFromWidget(this, WID_NGRFI_MAINPANEL, TOP_OFFSET + BOTTOM_OFFSET);
	}

	/**
	 * Some data on this window has become invalid.
	 * @param data Information about the changed data.
	 * @param gui_scope Whether the call is done from GUI scope. You may not do everything when not in GUI scope. See #InvalidateWindowData() for details.
	 */
	virtual void OnInvalidateData(int data = 0, bool gui_scope = true)
	{
		if (!gui_scope) return;
		if (this->HasChainIndex()) {
			this->ValidateChainIndex();
			this->SetWidgetDisabledState(WID_NGRFI_VEH_PREV, this->chain_index == 0);
			Vehicle *v = Vehicle::Get(this->GetFeatureIndex());
			this->SetWidgetDisabledState(WID_NGRFI_VEH_NEXT, v == NULL || v->Next() == NULL);
		}
	}
};

/* static */ std::map<NewGRFInspectWindow::RowKey, NIParameters> NewGRFInspectWindow::varparams[GSF_FAKE_END];

#define NWID_PARAMETER_ROW(nip) \
	NWidget(NWID_SELECTION, INVALID_COLOUR, WID_NGRFI_PARAM_SEL + (nip)), \
		NWidget(NWID_HORIZONTAL, NC_EQUALSIZE), SetPIP(WD_FRAMERECT_LEFT, 0, 0), \
			NWidget(WWT_TEXT, COLOUR_GREY, WID_NGRFI_PARAM_LABEL + (nip)), SetDataTip(STR_BLACK_RAW_STRING, STR_NULL), SetFill(1, 0), SetResize(1, 0), \
			NWidget(WWT_EDITBOX, COLOUR_GREY, WID_NGRFI_PARAM_EDIT + (nip)), SetFill(1, 0), SetResize(1, 0), \
		EndContainer(), \
	EndContainer()

static const NWidgetPart _nested_newgrf_inspect_chain_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY),
		NWidget(WWT_CAPTION, COLOUR_GREY, WID_NGRFI_CAPTION), SetDataTip(STR_NEWGRF_INSPECT_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_SHADEBOX, COLOUR_GREY),
		NWidget(WWT_DEFSIZEBOX, COLOUR_GREY),
		NWidget(WWT_STICKYBOX, COLOUR_GREY),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_GREY),
		NWidget(NWID_HORIZONTAL),
			NWidget(WWT_PUSHARROWBTN, COLOUR_GREY, WID_NGRFI_VEH_PREV), SetDataTip(AWV_DECREASE, STR_NULL),
			NWidget(WWT_PUSHARROWBTN, COLOUR_GREY, WID_NGRFI_VEH_NEXT), SetDataTip(AWV_INCREASE, STR_NULL),
			NWidget(WWT_EMPTY, COLOUR_GREY, WID_NGRFI_VEH_CHAIN), SetFill(1, 0), SetResize(1, 0),
		EndContainer(),
	EndContainer(),
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_PANEL, COLOUR_GREY, WID_NGRFI_MAINPANEL), SetMinimalSize(300, 0), SetScrollbar(WID_NGRFI_SCROLLBAR), EndContainer(),
		NWidget(NWID_VSCROLLBAR, COLOUR_GREY, WID_NGRFI_SCROLLBAR),
	EndContainer(),
	NWidget(NWID_HORIZONTAL),
		NWidget(NWID_HORIZONTAL, NC_EQUALSIZE),
			NWidget(WWT_PANEL, COLOUR_GREY),
				NWID_PARAMETER_ROW(NIP_VAR60),
				NWID_PARAMETER_ROW(NIP_VAR10),
				NWID_PARAMETER_ROW(NIP_VAR18),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x00),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x01),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x02),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x03),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x04),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x05),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x06),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x07),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x08),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x09),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x0A),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x0B),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x0C),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x0D),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x0E),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x0F),
			EndContainer(),
			NWidget(WWT_PANEL, COLOUR_GREY, WID_NGRFI_RESULT), SetFill(1, 1), SetResize(1, 0),
			EndContainer(),
		EndContainer(),
		NWidget(WWT_PANEL, COLOUR_GREY),
			NWidget(NWID_SPACER), SetFill(0, 1),
			NWidget(WWT_RESIZEBOX, COLOUR_GREY),
		EndContainer(),
	EndContainer(),
};

static const NWidgetPart _nested_newgrf_inspect_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY),
		NWidget(WWT_CAPTION, COLOUR_GREY, WID_NGRFI_CAPTION), SetDataTip(STR_NEWGRF_INSPECT_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_NGRFI_PARENT), SetDataTip(STR_NEWGRF_INSPECT_PARENT_BUTTON, STR_NEWGRF_INSPECT_PARENT_TOOLTIP),
		NWidget(WWT_SHADEBOX, COLOUR_GREY),
		NWidget(WWT_DEFSIZEBOX, COLOUR_GREY),
		NWidget(WWT_STICKYBOX, COLOUR_GREY),
	EndContainer(),
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_PANEL, COLOUR_GREY, WID_NGRFI_MAINPANEL), SetMinimalSize(300, 0), SetScrollbar(WID_NGRFI_SCROLLBAR), EndContainer(),
		NWidget(NWID_VSCROLLBAR, COLOUR_GREY, WID_NGRFI_SCROLLBAR),
	EndContainer(),
	NWidget(NWID_HORIZONTAL),
		NWidget(NWID_HORIZONTAL, NC_EQUALSIZE),
			NWidget(WWT_PANEL, COLOUR_GREY),
				NWID_PARAMETER_ROW(NIP_VAR60),
				NWID_PARAMETER_ROW(NIP_VAR10),
				NWID_PARAMETER_ROW(NIP_VAR18),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x00),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x01),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x02),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x03),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x04),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x05),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x06),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x07),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x08),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x09),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x0A),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x0B),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x0C),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x0D),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x0E),
				NWID_PARAMETER_ROW(NIP_REG100 + 0x0F),
			EndContainer(),
			NWidget(WWT_PANEL, COLOUR_GREY, WID_NGRFI_RESULT), SetFill(1, 1), SetResize(1, 0),
			EndContainer(),
		EndContainer(),
		NWidget(WWT_PANEL, COLOUR_GREY),
			NWidget(NWID_SPACER), SetFill(0, 1),
			NWidget(WWT_RESIZEBOX, COLOUR_GREY),
		EndContainer(),
	EndContainer(),
};

static WindowDesc _newgrf_inspect_chain_desc(
	WDP_AUTO, "newgrf_inspect_chain", 400, 300,
	WC_NEWGRF_INSPECT, WC_NONE,
	0,
	_nested_newgrf_inspect_chain_widgets, lengthof(_nested_newgrf_inspect_chain_widgets)
);

static WindowDesc _newgrf_inspect_desc(
	WDP_AUTO, "newgrf_inspect", 400, 300,
	WC_NEWGRF_INSPECT, WC_NONE,
	0,
	_nested_newgrf_inspect_widgets, lengthof(_nested_newgrf_inspect_widgets)
);

/**
 * Show the inspect window for a given feature and index.
 * The index is normally an in-game location/identifier, such
 * as a TileIndex or an IndustryID depending on the feature
 * we want to inspect.
 * @param feature The feature we want to inspect.
 * @param index   The index/identifier of the feature to inspect.
 * @param grfid   GRFID of the item opening this window, or 0 if not opened by other window.
 */
void ShowNewGRFInspectWindow(GrfSpecFeature feature, uint index, const uint32 grfid)
{
	if (!IsNewGRFInspectable(feature, index)) return;

	WindowNumber wno = GetInspectWindowNumber(feature, index);
	NewGRFInspectWindow *w = AllocateWindowDescFront<NewGRFInspectWindow>(feature == GSF_TRAINS || feature == GSF_ROADVEHICLES ? &_newgrf_inspect_chain_desc : &_newgrf_inspect_desc, wno);
	if (w == NULL) w = (NewGRFInspectWindow *)FindWindowById(WC_NEWGRF_INSPECT, wno);
	w->SetCallerGRFID(grfid);
}

/**
 * Invalidate the inspect window for a given feature and index.
 * The index is normally an in-game location/identifier, such
 * as a TileIndex or an IndustryID depending on the feature
 * we want to inspect.
 * @param feature The feature we want to invalidate the window for.
 * @param index   The index/identifier of the feature to invalidate.
 */
void InvalidateNewGRFInspectWindow(GrfSpecFeature feature, uint index)
{
	if (feature == GSF_INVALID) return;

	WindowNumber wno = GetInspectWindowNumber(feature, index);
	InvalidateWindowData(WC_NEWGRF_INSPECT, wno);
}

/**
 * Delete inspect window for a given feature and index.
 * The index is normally an in-game location/identifier, such
 * as a TileIndex or an IndustryID depending on the feature
 * we want to inspect.
 * @param feature The feature we want to delete the window for.
 * @param index   The index/identifier of the feature to delete.
 */
void DeleteNewGRFInspectWindow(GrfSpecFeature feature, uint index)
{
	if (feature == GSF_INVALID) return;

	WindowNumber wno = GetInspectWindowNumber(feature, index);
	DeleteWindowById(WC_NEWGRF_INSPECT, wno);

	/* Reinitialise the land information window to remove the "debug" sprite if needed.
	 * Note: Since we might be called from a command here, it is important to not execute
	 * the invalidation immediately. The landinfo window tests commands itself. */
	InvalidateWindowData(WC_LAND_INFO, 0, 1);
}

/**
 * Can we inspect the data given a certain feature and index.
 * The index is normally an in-game location/identifier, such
 * as a TileIndex or an IndustryID depending on the feature
 * we want to inspect.
 * @param feature The feature we want to inspect.
 * @param index   The index/identifier of the feature to inspect.
 * @return true if there is something to show.
 */
bool IsNewGRFInspectable(GrfSpecFeature feature, uint index)
{
	const NIFeature *nif = GetFeature(GetInspectWindowNumber(feature, index));
	if (nif == NULL) return false;
	return nif->helper->IsInspectable(index);
}

/**
 * Get the GrfSpecFeature associated with the tile.
 * @param tile The tile to get the feature from.
 * @return the GrfSpecFeature.
 */
GrfSpecFeature GetGrfSpecFeature(TileIndex tile)
{
	switch (GetTileType(tile)) {
		default:              return GSF_INVALID;
		case MP_RAILWAY:      return GSF_RAILTYPES;
		case MP_ROAD:         return IsLevelCrossing(tile) ? GSF_RAILTYPES : GSF_INVALID;
		case MP_HOUSE:        return GSF_HOUSES;
		case MP_INDUSTRY:     return GSF_INDUSTRYTILES;
		case MP_OBJECT:       return GSF_OBJECTS;

		case MP_STATION:
			switch (GetStationType(tile)) {
				case STATION_RAIL:    return GSF_STATIONS;
				case STATION_AIRPORT: return GSF_AIRPORTTILES;
				default:              return GSF_INVALID;
			}
	}
}

/**
 * Get the GrfSpecFeature associated with the vehicle.
 * @param type The vehicle type to get the feature from.
 * @return the GrfSpecFeature.
 */
GrfSpecFeature GetGrfSpecFeature(VehicleType type)
{
	switch (type) {
		case VEH_TRAIN:    return GSF_TRAINS;
		case VEH_ROAD:     return GSF_ROADVEHICLES;
		case VEH_SHIP:     return GSF_SHIPS;
		case VEH_AIRCRAFT: return GSF_AIRCRAFT;
		default:           return GSF_INVALID;
	}
}



/**** Sprite Aligner ****/

/** Window used for aligning sprites. */
struct SpriteAlignerWindow : Window {
	SpriteID current_sprite; ///< The currently shown sprite
	Scrollbar *vscroll;

	SpriteAlignerWindow(WindowDesc *desc, WindowNumber wno) : Window(desc)
	{
		this->CreateNestedTree();
		this->vscroll = this->GetScrollbar(WID_SA_SCROLLBAR);
		this->FinishInitNested(wno);

		/* Oh yes, we assume there is at least one normal sprite! */
		while (GetSpriteType(this->current_sprite) != ST_NORMAL) this->current_sprite++;
	}

	virtual void SetStringParameters(int widget) const
	{
		switch (widget) {
			case WID_SA_CAPTION:
				SetDParam(0, this->current_sprite);
				SetDParamStr(1, FioGetFilename(GetOriginFileSlot(this->current_sprite)));
				break;

			case WID_SA_OFFSETS: {
				const Sprite *spr = GetSprite(this->current_sprite, ST_NORMAL);
				SetDParam(0, spr->x_offs / ZOOM_LVL_BASE);
				SetDParam(1, spr->y_offs / ZOOM_LVL_BASE);
				break;
			}

			default:
				break;
		}
	}

	virtual void UpdateWidgetSize(int widget, Dimension *size, const Dimension &padding, Dimension *fill, Dimension *resize)
	{
		if (widget != WID_SA_LIST) return;

		resize->height = max(11, FONT_HEIGHT_NORMAL + 1);
		resize->width  = 1;

		/* Resize to about 200 pixels (for the preview) */
		size->height = (1 + 200 / resize->height) * resize->height;
	}

	virtual void DrawWidget(const Rect &r, int widget) const
	{
		switch (widget) {
			case WID_SA_SPRITE: {
				/* Center the sprite ourselves */
				const Sprite *spr = GetSprite(this->current_sprite, ST_NORMAL);
				int width  = r.right  - r.left + 1;
				int height = r.bottom - r.top  + 1;
				int x = r.left - spr->x_offs / ZOOM_LVL_BASE + (width  - spr->width / ZOOM_LVL_BASE) / 2;
				int y = r.top  - spr->y_offs / ZOOM_LVL_BASE + (height - spr->height / ZOOM_LVL_BASE) / 2;

				/* And draw only the part within the sprite area */
				SubSprite subspr = {
					spr->x_offs + (spr->width  - width  * ZOOM_LVL_BASE) / 2 + 1,
					spr->y_offs + (spr->height - height * ZOOM_LVL_BASE) / 2 + 1,
					spr->x_offs + (spr->width  + width  * ZOOM_LVL_BASE) / 2 - 1,
					spr->y_offs + (spr->height + height * ZOOM_LVL_BASE) / 2 - 1,
				};

				DrawSprite(this->current_sprite, PAL_NONE, x, y, &subspr, ZOOM_LVL_GUI);
				break;
			}

			case WID_SA_LIST: {
				const NWidgetBase *nwid = this->GetWidget<NWidgetBase>(widget);
				int step_size = nwid->resize_y;

				SmallVector<SpriteID, 256> &list = _newgrf_debug_sprite_picker.sprites;
				int max = min<int>(this->vscroll->GetPosition() + this->vscroll->GetCapacity(), list.Length());

				int y = r.top + WD_FRAMERECT_TOP;
				for (int i = this->vscroll->GetPosition(); i < max; i++) {
					SetDParam(0, list[i]);
					DrawString(r.left + WD_FRAMERECT_LEFT, r.right - WD_FRAMERECT_RIGHT, y, STR_BLACK_COMMA, TC_FROMSTRING, SA_RIGHT | SA_FORCE);
					y += step_size;
				}
				break;
			}
		}
	}

	virtual void OnClick(Point pt, int widget, int click_count)
	{
		switch (widget) {
			case WID_SA_PREVIOUS:
				do {
					this->current_sprite = (this->current_sprite == 0 ? GetMaxSpriteID() :  this->current_sprite) - 1;
				} while (GetSpriteType(this->current_sprite) != ST_NORMAL);
				this->SetDirty();
				break;

			case WID_SA_GOTO:
				ShowQueryString(STR_EMPTY, STR_SPRITE_ALIGNER_GOTO_CAPTION, 7, this, CS_NUMERAL, QSF_NONE);
				break;

			case WID_SA_NEXT:
				do {
					this->current_sprite = (this->current_sprite + 1) % GetMaxSpriteID();
				} while (GetSpriteType(this->current_sprite) != ST_NORMAL);
				this->SetDirty();
				break;

			case WID_SA_PICKER:
				this->LowerWidget(WID_SA_PICKER);
				_newgrf_debug_sprite_picker.mode = SPM_WAIT_CLICK;
				this->SetDirty();
				break;

			case WID_SA_LIST: {
				const NWidgetBase *nwid = this->GetWidget<NWidgetBase>(widget);
				int step_size = nwid->resize_y;

				uint i = this->vscroll->GetPosition() + (pt.y - nwid->pos_y) / step_size;
				if (i < _newgrf_debug_sprite_picker.sprites.Length()) {
					SpriteID spr = _newgrf_debug_sprite_picker.sprites[i];
					if (GetSpriteType(spr) == ST_NORMAL) this->current_sprite = spr;
				}
				this->SetDirty();
				break;
			}

			case WID_SA_UP:
			case WID_SA_DOWN:
			case WID_SA_LEFT:
			case WID_SA_RIGHT: {
				/*
				 * Yes... this is a hack.
				 *
				 * No... I don't think it is useful to make this less of a hack.
				 *
				 * If you want to align sprites, you just need the number. Generally
				 * the sprite caches are big enough to not remove the sprite from the
				 * cache. If that's not the case, just let the NewGRF developer
				 * increase the cache size instead of storing thousands of offsets
				 * for the incredibly small chance that it's actually going to be
				 * used by someone and the sprite cache isn't big enough for that
				 * particular NewGRF developer.
				 */
				Sprite *spr = const_cast<Sprite *>(GetSprite(this->current_sprite, ST_NORMAL));
				switch (widget) {
					case WID_SA_UP:    spr->y_offs -= ZOOM_LVL_BASE; break;
					case WID_SA_DOWN:  spr->y_offs += ZOOM_LVL_BASE; break;
					case WID_SA_LEFT:  spr->x_offs -= ZOOM_LVL_BASE; break;
					case WID_SA_RIGHT: spr->x_offs += ZOOM_LVL_BASE; break;
				}
				/* Of course, we need to redraw the sprite, but where is it used?
				 * Everywhere is a safe bet. */
				MarkWholeScreenDirty();
				break;
			}
		}
	}

	virtual void OnQueryTextFinished(char *str)
	{
		if (StrEmpty(str)) return;

		this->current_sprite = atoi(str);
		if (this->current_sprite >= GetMaxSpriteID()) this->current_sprite = 0;
		while (GetSpriteType(this->current_sprite) != ST_NORMAL) {
			this->current_sprite = (this->current_sprite + 1) % GetMaxSpriteID();
		}
		this->SetDirty();
	}

	/**
	 * Some data on this window has become invalid.
	 * @param data Information about the changed data.
	 * @param gui_scope Whether the call is done from GUI scope. You may not do everything when not in GUI scope. See #InvalidateWindowData() for details.
	 */
	virtual void OnInvalidateData(int data = 0, bool gui_scope = true)
	{
		if (!gui_scope) return;
		if (data == 1) {
			/* Sprite picker finished */
			this->RaiseWidget(WID_SA_PICKER);
			this->vscroll->SetCount(_newgrf_debug_sprite_picker.sprites.Length());
		}
	}

	virtual void OnResize()
	{
		this->vscroll->SetCapacityFromWidget(this, WID_SA_LIST);
	}
};

static const NWidgetPart _nested_sprite_aligner_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY),
		NWidget(WWT_CAPTION, COLOUR_GREY, WID_SA_CAPTION), SetDataTip(STR_SPRITE_ALIGNER_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_SHADEBOX, COLOUR_GREY),
		NWidget(WWT_STICKYBOX, COLOUR_GREY),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_GREY),
		NWidget(NWID_HORIZONTAL), SetPIP(0, 0, 10),
			NWidget(NWID_VERTICAL), SetPIP(10, 5, 10),
				NWidget(NWID_HORIZONTAL, NC_EQUALSIZE), SetPIP(10, 5, 10),
					NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_SA_PREVIOUS), SetDataTip(STR_SPRITE_ALIGNER_PREVIOUS_BUTTON, STR_SPRITE_ALIGNER_PREVIOUS_TOOLTIP), SetFill(1, 0),
					NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_SA_GOTO), SetDataTip(STR_SPRITE_ALIGNER_GOTO_BUTTON, STR_SPRITE_ALIGNER_GOTO_TOOLTIP), SetFill(1, 0),
					NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, WID_SA_NEXT), SetDataTip(STR_SPRITE_ALIGNER_NEXT_BUTTON, STR_SPRITE_ALIGNER_NEXT_TOOLTIP), SetFill(1, 0),
				EndContainer(),
				NWidget(NWID_HORIZONTAL), SetPIP(10, 5, 10),
					NWidget(NWID_SPACER), SetFill(1, 1),
					NWidget(WWT_PUSHIMGBTN, COLOUR_GREY, WID_SA_UP), SetDataTip(SPR_ARROW_UP, STR_SPRITE_ALIGNER_MOVE_TOOLTIP), SetResize(0, 0),
					NWidget(NWID_SPACER), SetFill(1, 1),
				EndContainer(),
				NWidget(NWID_HORIZONTAL_LTR), SetPIP(10, 5, 10),
					NWidget(NWID_VERTICAL),
						NWidget(NWID_SPACER), SetFill(1, 1),
						NWidget(WWT_PUSHIMGBTN, COLOUR_GREY, WID_SA_LEFT), SetDataTip(SPR_ARROW_LEFT, STR_SPRITE_ALIGNER_MOVE_TOOLTIP), SetResize(0, 0),
						NWidget(NWID_SPACER), SetFill(1, 1),
					EndContainer(),
					NWidget(WWT_PANEL, COLOUR_DARK_BLUE, WID_SA_SPRITE), SetDataTip(STR_NULL, STR_SPRITE_ALIGNER_SPRITE_TOOLTIP),
					EndContainer(),
					NWidget(NWID_VERTICAL),
						NWidget(NWID_SPACER), SetFill(1, 1),
						NWidget(WWT_PUSHIMGBTN, COLOUR_GREY, WID_SA_RIGHT), SetDataTip(SPR_ARROW_RIGHT, STR_SPRITE_ALIGNER_MOVE_TOOLTIP), SetResize(0, 0),
						NWidget(NWID_SPACER), SetFill(1, 1),
					EndContainer(),
				EndContainer(),
				NWidget(NWID_HORIZONTAL), SetPIP(10, 5, 10),
					NWidget(NWID_SPACER), SetFill(1, 1),
					NWidget(WWT_PUSHIMGBTN, COLOUR_GREY, WID_SA_DOWN), SetDataTip(SPR_ARROW_DOWN, STR_SPRITE_ALIGNER_MOVE_TOOLTIP), SetResize(0, 0),
					NWidget(NWID_SPACER), SetFill(1, 1),
				EndContainer(),
				NWidget(NWID_HORIZONTAL), SetPIP(10, 5, 10),
					NWidget(WWT_LABEL, COLOUR_GREY, WID_SA_OFFSETS), SetDataTip(STR_SPRITE_ALIGNER_OFFSETS, STR_NULL), SetFill(1, 0),
				EndContainer(),
			EndContainer(),
			NWidget(NWID_VERTICAL), SetPIP(10, 5, 10),
				NWidget(WWT_TEXTBTN, COLOUR_GREY, WID_SA_PICKER), SetDataTip(STR_SPRITE_ALIGNER_PICKER_BUTTON, STR_SPRITE_ALIGNER_PICKER_TOOLTIP), SetFill(1, 0),
				NWidget(NWID_HORIZONTAL),
					NWidget(WWT_MATRIX, COLOUR_GREY, WID_SA_LIST), SetResize(1, 1), SetMatrixDataTip(1, 0, STR_NULL), SetFill(1, 1), SetScrollbar(WID_SA_SCROLLBAR),
					NWidget(NWID_VSCROLLBAR, COLOUR_GREY, WID_SA_SCROLLBAR),
				EndContainer(),
			EndContainer(),
		EndContainer(),
	EndContainer(),
};

static WindowDesc _sprite_aligner_desc(
	WDP_AUTO, "sprite_aligner", 400, 300,
	WC_SPRITE_ALIGNER, WC_NONE,
	0,
	_nested_sprite_aligner_widgets, lengthof(_nested_sprite_aligner_widgets)
);

/**
 * Show the window for aligning sprites.
 */
void ShowSpriteAlignerWindow()
{
	AllocateWindowDescFront<SpriteAlignerWindow>(&_sprite_aligner_desc, 0);
}
