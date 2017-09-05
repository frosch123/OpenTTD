/* $Id: newgrf_spritegroup.h 22731 2011-08-08 21:26:58Z frosch $ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file newgrf_profiler.h Profiling NewGRFs. */

#ifndef NEWGRF_PROFILER_H
#define NEWGRF_PROFILER_H

#include "debug.h"
#include "newgrf_callbacks.h"

struct NewGRFProfiler {
	static void Start();
	static void Stop();
	static void Reset();
	static bool IsEnabled();
	static void OnNewMonth();

	static void Record(uint8 feature, uint32 grfid, uint entity_id, CallbackID callback, uint64 ms);
};

void ShowNewGRFProfilerWindow();

#endif /* NEWGRF_PROFILER_H */
