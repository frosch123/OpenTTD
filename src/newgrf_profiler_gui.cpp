/* $Id: newgrf_industries.cpp 22656 2011-07-11 16:32:19Z michi_cc $ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file newgrf_profiler_gui.cpp GUIs for profiling NewGRFs. */

#include "stdafx.h"
#include "core/enum_type.hpp"
#include "date_func.h"
#include "newgrf_profiler.h"
#include "genworld.h"
#include "window_gui.h"
#include "window_func.h"
#include "strings_func.h"
#include "gfx_func.h"
#include "sortlist_type.h"
#include "newgrf.h"
#include "industrytype.h"
#include "engine_base.h"
#include "newgrf_airport.h"
#include "newgrf_object.h"
#include "newgrf_canal.h"
#include "rail.h"

#include "table/strings.h"

#include <map>

static const char * const _water_feature_names[] =  {
	"Watercliffs",
	"Locks",
	"Dikes",
	"Icons",
	"Flat docks",
	"River slope",
	"Riverbanks",
	"River GUI",
	"Buoy",
};
assert_compile(lengthof(_water_feature_names) == CF_END);

static bool _profiler_enabled = true;

enum ProfilerScope {
	SCOPE_BEGIN = 0,
	MAPGEN = SCOPE_BEGIN,
	JANUARY,
	ALL_MONTHS = JANUARY + 12,

	SCOPE_END
};
DECLARE_POSTFIX_INCREMENT(ProfilerScope)

struct EntityKey {
	CallbackID callback;
	uint8 feature;
	uint32 grfid;
	uint entity_id;

	bool operator < (const EntityKey &other) const
	{
		if (this->callback != other.callback) return this->callback < other.callback;
		if (this->grfid != other.grfid) return this->grfid < other.grfid;
		if (this->feature != other.feature) return this->feature < other.feature;
		return this->entity_id < other.entity_id;
	}

	bool HasEntities() const
	{
		switch (feature) {
			case GSF_SOUNDFX:
				return false;

			case GSF_TRAINS:
			case GSF_ROADVEHICLES:
			case GSF_SHIPS:
			case GSF_AIRCRAFT:
			case GSF_STATIONS:
			case GSF_CANALS:
			case GSF_HOUSES:
			case GSF_INDUSTRYTILES:
			case GSF_INDUSTRIES:
			case GSF_CARGOS:
			case GSF_AIRPORTS:
			case GSF_OBJECTS:
			case GSF_RAILTYPES:
			case GSF_AIRPORTTILES:
				return true;

			default: NOT_REACHED();
		}
	}

	uint GetEntityCount() const
	{
		return 1; // TODO
	}

	StringID GetEntityName() const
	{
		switch (feature) {
			case GSF_TRAINS:
			case GSF_ROADVEHICLES:
			case GSF_SHIPS:
			case GSF_AIRCRAFT:
				SetDParam(0, this->entity_id);
				return STR_ENGINE_NAME;

			case GSF_STATIONS:
				return STR_EMPTY; // TODO

			case GSF_CANALS:
				assert(this->entity_id < lengthof(_water_feature_names));
				SetDParamStr(0, _water_feature_names[this->entity_id]);
				return STR_JUST_RAW_STRING;

			case GSF_HOUSES:
				return STR_EMPTY; // TODO

			case GSF_INDUSTRYTILES:
				return STR_EMPTY; // TODO

			case GSF_INDUSTRIES:
				return GetIndustrySpec(this->entity_id)->name;

			case GSF_CARGOS:
				return CargoSpec::Get(this->entity_id)->name;

			case GSF_AIRPORTS:
				return AirportSpec::Get(this->entity_id)->name;

			case GSF_OBJECTS:
				return ObjectSpec::Get(this->entity_id)->name;

			case GSF_RAILTYPES:
				return GetRailTypeInfo((RailType)this->entity_id)->strings.menu_text;

			case GSF_AIRPORTTILES:
				return STR_EMPTY; // TODO

			default: NOT_REACHED();
		}
	}
};

struct Statistics {
	uint num_calls;
	uint64 total_time;

	Statistics &operator += (const Statistics &other)
	{
		this->num_calls += other.num_calls;
		this->total_time += other.total_time;
		return *this;
	}
};
typedef std::map<EntityKey, Statistics> StatisticsMap;
typedef std::map<CallbackID, Statistics> FilteredMap;

static StatisticsMap _profile[SCOPE_END];
static ProfilerScope _selected_sort_column = ALL_MONTHS;


/* static */ void NewGRFProfiler::Start()
{
	_profiler_enabled = true;
	InvalidateWindowClassesData(WC_NEWGRF_PROFILER);
}

/* static */ void NewGRFProfiler::Stop()
{
	_profiler_enabled = false;
	InvalidateWindowClassesData(WC_NEWGRF_PROFILER);
}

/* static */ void NewGRFProfiler::Reset()
{
	for (ProfilerScope s = SCOPE_BEGIN; s < SCOPE_END; s++) {
		_profile[s].clear();
	}
	InvalidateWindowClassesData(WC_NEWGRF_PROFILER);
}

/* static */ bool NewGRFProfiler::IsEnabled()
{
	return _profiler_enabled;
}

/* static */ void NewGRFProfiler::OnNewMonth()
{
	StatisticsMap &s = _profile[JANUARY + _cur_month];
	StatisticsMap &all = _profile[ALL_MONTHS];
	for (StatisticsMap::iterator it = s.begin(); it != s.end(); it++) {
		Statistics &sum = all[it->first];
		sum.num_calls -= it->second.num_calls;
		sum.total_time -= it->second.total_time;
	}
	s.clear();
	InvalidateWindowClassesData(WC_NEWGRF_PROFILER);
}

/* static */ void NewGRFProfiler::Record(uint8 feature, uint32 grfid, uint entity_id, CallbackID callback, uint64 ms)
{
	EntityKey key = {callback, feature, grfid, entity_id};
	Statistics &s = _profile[_generating_world ? MAPGEN : JANUARY + _cur_month][key];
	s.num_calls++;
	s.total_time += ms;
	if (!_generating_world) {
		Statistics& s = _profile[ALL_MONTHS][key];
		s.num_calls++;
		s.total_time += ms;
	}
}



enum NewGRFProfilerWidgets {
	NPW_STARTSTOP,
	NPW_LABEL,
	NPW_MAPGEN,
	NPW_MONTHS,
	NPW_ALL_MONTHS = NPW_MONTHS + 12,
	NPW_TABLE,
	NPW_SCROLLBAR,
};

static FilteredMap filtered_data[SCOPE_END];

static int CDECL SortByNumCalls(const CallbackID *a, const CallbackID *b)
{
	FilteredMap &profile = filtered_data[_selected_sort_column];
	FilteredMap::iterator ita = profile.find(*a);
	FilteredMap::iterator itb = profile.find(*b);
	if (ita != profile.end() && itb != profile.end()) {
		return itb->second.num_calls - ita->second.num_calls;
	} else {
		if (ita != profile.end()) return -ita->second.num_calls;
		if (itb != profile.end()) return itb->second.num_calls;
		return *b - *a;
	}
}

struct NewGRFProfilerWindow : Window {
	Scrollbar *vscroll;
	GUIList<CallbackID> rows;

	void SortList()
	{
		this->rows.Sort(&SortByNumCalls);
	}

	void BuildList()
	{
		rows.Clear();
		for (ProfilerScope scope = SCOPE_BEGIN; scope < SCOPE_END; scope++) {
			filtered_data[scope].clear();
			for (StatisticsMap::iterator it = _profile[scope].begin(); it != _profile[scope].end(); it++) {
				if (it->second.num_calls == 0) continue;

				// TODO filter

				filtered_data[scope][it->first.callback] += it->second;
			}
			for (FilteredMap::iterator it = filtered_data[scope].begin(); it != filtered_data[scope].end(); it++) {
				rows.Include(it->first);
			}
		}
		this->vscroll->SetCount(rows.Length());
		this->rows.RebuildDone();
		this->SortList();
	}

	NewGRFProfilerWindow(const WindowDesc *desc, WindowNumber wno) : Window()
	{
		this->CreateNestedTree(desc);
		this->vscroll = this->GetScrollbar(NPW_SCROLLBAR);
		this->FinishInitNested(desc, wno);

		this->InvalidateData();
	}

	virtual void OnInvalidateData(int data, bool gui_scope)
	{
		BuildList();
	}

	virtual void SetStringParameters(int widget) const
	{
		if (widget >= NPW_MONTHS && widget < NPW_MONTHS + 12) {
			SetDParam(0, STR_MONTH_ABBREV_JAN + widget - NPW_MONTHS);
			return;
		}
		switch (widget) {
			case NPW_STARTSTOP: {
				SetDParam(0, NewGRFProfiler::IsEnabled() ? STR_NEWGRF_PROFILER_STOP : STR_NEWGRF_PROFILER_START);
				break;
			}
		}
	}

	virtual void UpdateWidgetSize(int widget, Dimension *size, const Dimension &padding, Dimension *fill, Dimension *resize)
	{
		if (widget >= NPW_MAPGEN && widget <= NPW_ALL_MONTHS) {
			const NWidgetCore *wid = this->GetWidget<NWidgetCore>(widget);
			SetDParam(0, STR_MONTH_ABBREV_JAN + widget - NPW_MONTHS);
			uint w = GetStringBoundingBox(wid->widget_data).width;
			SetDParam(0, 99999999);
			w = max(w, GetStringBoundingBox(STR_BLACK_COMMA).width);
			size->width = w + 2 * WD_SORTBUTTON_ARROW_WIDTH + WD_INSET_LEFT + WD_INSET_RIGHT;
			return;
		};

		switch (widget) {
			case NPW_LABEL: {
				SetDParam(0, 0x999);
				uint w = GetStringBoundingBox(STR_NEWGRF_PROFILER_CALLBACK).width;
				w = max(w, GetStringBoundingBox(STR_NEWGRF_PROFILER_NORMAL).width);
				w = max(w, GetStringBoundingBox(STR_NEWGRF_PROFILER_TRIGGER).width);
				size->width = w + WD_INSET_LEFT + WD_INSET_RIGHT;
				break;
			} 

			case NPW_TABLE:
				resize->height = 2 * max(11, FONT_HEIGHT_NORMAL + 1);
				size->height = 5 * resize->height;
				break;
		}
	}

	virtual void OnClick(Point pt, int widget, int click_count)
	{
		if (widget >= NPW_MAPGEN && widget <= NPW_ALL_MONTHS) {
			if (widget == NPW_MAPGEN + _selected_sort_column) {
				this->rows.ToggleSortOrder();
			} else {
				_selected_sort_column = (ProfilerScope)(widget - NPW_MAPGEN);
				this->rows.ForceResort();
				this->SortList();
			}
			this->SetDirty();
			return;
		}

		switch (widget) {
			case NPW_STARTSTOP: {
				if (NewGRFProfiler::IsEnabled()) {
					NewGRFProfiler::Stop();
				} else {
					NewGRFProfiler::Start();
				}
				break;
			}
		}
	}

	virtual void DrawWidget(const Rect &r, int widget) const
	{
		if (widget >= NPW_MAPGEN && widget <= NPW_ALL_MONTHS) {
			if (widget == NPW_MAPGEN + _selected_sort_column) DrawSortButtonState(widget, this->rows.IsDescSortOrder() ? SBS_DOWN : SBS_UP);
		}
		if (widget != NPW_TABLE) return;

		uint row_height = this->GetWidget<NWidgetBase>(NPW_TABLE)->resize_y;
		for (uint i = 0; i < this->vscroll->GetCapacity(); i++) {
			uint pos = this->vscroll->GetPosition() + i;
			if (pos >= this->rows.Length()) break;
			CallbackID cb = this->rows[pos];

			int y = r.top + i * row_height + WD_INSET_TOP;
			int y2 = y + FONT_HEIGHT_NORMAL;
			SetDParam(0, cb);
			StringID str = STR_NEWGRF_PROFILER_CALLBACK;
			switch (cb) {
				case CBID_NO_CALLBACK:    str = STR_NEWGRF_PROFILER_NORMAL; break;
				case CBID_RANDOM_TRIGGER: str = STR_NEWGRF_PROFILER_TRIGGER; break;
				default: break;
			}
			const NWidgetBase *wid = this->GetWidget<NWidgetBase>(NPW_LABEL);
			int left = wid->pos_x + WD_INSET_LEFT;
			int right = wid->pos_x + wid->current_x - 1 - WD_INSET_RIGHT;
			DrawString(left, right, y, str);

			for (uint j = 0; j < SCOPE_END; j++) {
				FilteredMap::iterator it = filtered_data[j].find(cb);
				if (it == filtered_data[j].end()) continue;

				const NWidgetBase *wid = this->GetWidget<NWidgetBase>(NPW_MAPGEN + j);
				int left = wid->pos_x + WD_INSET_LEFT;
				int right = wid->pos_x + wid->current_x - 1 - WD_INSET_RIGHT;

				SetDParam(0, it->second.num_calls);
				DrawString(left, right, y, STR_BLACK_COMMA, TC_FROMSTRING, SA_RIGHT | SA_FORCE);
				SetDParam(0, it->second.total_time / 1000000);
				DrawString(left, right, y2, STR_BLACK_COMMA, TC_FROMSTRING, SA_RIGHT | SA_FORCE);
			}
		}
	}

	virtual void OnHundredthTick()
	{
		if (NewGRFProfiler::IsEnabled()) this->InvalidateData();
	}

	virtual void OnResize()
	{
		this->vscroll->SetCapacityFromWidget(this, NPW_TABLE);
		this->GetWidget<NWidgetCore>(NPW_TABLE)->widget_data = (this->vscroll->GetCapacity() << MAT_ROW_START) + (1 << MAT_COL_START);
	}
};

static const NWidgetPart _nested_newgrf_profiler_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY),
		NWidget(WWT_CAPTION, COLOUR_GREY), SetDataTip(STR_NEWGRF_PROFILER_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_SHADEBOX, COLOUR_GREY),
		NWidget(WWT_STICKYBOX, COLOUR_GREY),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_GREY),
		NWidget(NWID_VERTICAL), SetPIP(5, 5, 0),
			NWidget(NWID_HORIZONTAL), SetPIP(10, 5, 10),
				NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_STARTSTOP), SetDataTip(STR_BLACK_STRING, STR_NEWGRF_PROFILER_STARTSTOP_TOOLTIP),
				NWidget(NWID_SPACER), SetResize(1, 0), SetFill(1, 1),
			EndContainer(),
			NWidget(NWID_HORIZONTAL),
				NWidget(NWID_VERTICAL),
					NWidget(NWID_HORIZONTAL),
						NWidget(WWT_EMPTY, INVALID_COLOUR, NPW_LABEL), SetResize(0, 0), SetFill(0, 1),
						NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_MAPGEN), SetDataTip(STR_NEWGRF_PROFILER_MAPGEN, STR_NULL), SetResize(0, 0), SetFill(1, 0),
						NWidget(NWID_HORIZONTAL, NC_EQUALSIZE),
							NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_MONTHS +  0), SetDataTip(STR_BLACK_STRING, STR_NULL), SetResize(1, 0), SetFill(1, 0),
							NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_MONTHS +  1), SetDataTip(STR_BLACK_STRING, STR_NULL), SetResize(1, 0), SetFill(1, 0),
							NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_MONTHS +  2), SetDataTip(STR_BLACK_STRING, STR_NULL), SetResize(1, 0), SetFill(1, 0),
							NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_MONTHS +  3), SetDataTip(STR_BLACK_STRING, STR_NULL), SetResize(1, 0), SetFill(1, 0),
							NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_MONTHS +  4), SetDataTip(STR_BLACK_STRING, STR_NULL), SetResize(1, 0), SetFill(1, 0),
							NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_MONTHS +  5), SetDataTip(STR_BLACK_STRING, STR_NULL), SetResize(1, 0), SetFill(1, 0),
							NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_MONTHS +  6), SetDataTip(STR_BLACK_STRING, STR_NULL), SetResize(1, 0), SetFill(1, 0),
							NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_MONTHS +  7), SetDataTip(STR_BLACK_STRING, STR_NULL), SetResize(1, 0), SetFill(1, 0),
							NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_MONTHS +  8), SetDataTip(STR_BLACK_STRING, STR_NULL), SetResize(1, 0), SetFill(1, 0),
							NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_MONTHS +  9), SetDataTip(STR_BLACK_STRING, STR_NULL), SetResize(1, 0), SetFill(1, 0),
							NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_MONTHS + 10), SetDataTip(STR_BLACK_STRING, STR_NULL), SetResize(1, 0), SetFill(1, 0),
							NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_MONTHS + 11), SetDataTip(STR_BLACK_STRING, STR_NULL), SetResize(1, 0), SetFill(1, 0),
						EndContainer(),
						NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, NPW_ALL_MONTHS), SetDataTip(STR_NEWGRF_PROFILER_ALL_MONTHS, STR_NULL), SetResize(0, 0), SetFill(1, 0),
					EndContainer(),
					NWidget(WWT_MATRIX, COLOUR_GREY, NPW_TABLE), SetMinimalSize(300, 50), SetDataTip(0x101, STR_NULL), SetResize(1, 1), SetFill(1, 1), SetScrollbar(NPW_SCROLLBAR),
				EndContainer(),
				NWidget(NWID_VERTICAL),
					NWidget(NWID_VSCROLLBAR, COLOUR_GREY, NPW_SCROLLBAR),
					NWidget(WWT_RESIZEBOX, COLOUR_GREY),
				EndContainer(),
			EndContainer(),
		EndContainer(),
	EndContainer(),
};

static const WindowDesc _newgrf_profiler_desc(
	WDP_AUTO, 600, 300,
	WC_NEWGRF_PROFILER, WC_NONE,
	WDF_UNCLICK_BUTTONS,
	_nested_newgrf_profiler_widgets, lengthof(_nested_newgrf_profiler_widgets)
);

void ShowNewGRFProfilerWindow()
{
	AllocateWindowDescFront<NewGRFProfilerWindow>(&_newgrf_profiler_desc, 0);
}
