package provide trajAnalysisTools 1.1.0

namespace eval ::trajAnalysisTools {
	variable lst_default_selTexts "{(all and not water)}"
	variable flg_debug 0
}

##################################
#	Attributions:
#---------------------------------
#	Author: Blake Riley <blake.riley@monash.edu>
#
#	Portions of this package are adapted from:
#	-	timeline.tcl <barryi@ks.uiuc.edu>
#	-	http://www.ks.uiuc.edu/Research/vmd/script_library/scripts/rmsd_matrix/rmsd_matrix.tcl <ltrabuco@ks.uiuc.edu>
#	
##################################
#	Syntax of this package
#---------------------------------
#	analyse function ?-arg var?...
#	<function>: 
#				rmsd, rmsf, dist, heatmap, ramachandran
#	<arg>:
#				-mol <molid> (default: top)
#				-sel "<selection text>" (default: "(all and not water)" )
#				-frames <begin:end> or <begin:step:end> or all (default: all)
#				-o <filename>
#
#	RMSF calc can take only one selection.
#	RMSD calc can take multiple selections.
#	If an output file is specified, it will be printed in CSV format,
#		otherwise it will be printed to stdout.
#
##################################
#	Procedures in this package:
#---------------------------------
#	-	analyse { function ?args? }
#	-	::trajAnalysisTools::helpMSG {}							\\ <null>
#	-	::trajAnalysisTools::parseArguments { rawArgs }			\\ dct_parsedArgs
#	-	::trajAnalysisTools::callFunction { args }				\\ <null>
#	-	::trajAnalysisTools::openFile { str_outFile }			\\ file_outFile
#	-	::trajAnalysisTools::closeFile { file_outFile }			\\ <null>
#	-	::trajAnalysisTools::CSVprint { file_output tbl_data }	\\ <null>
#	-	::trajAnalysisTools::stdoutprint { tbl_data }			\\ <null>
#	-	::trajAnalysisTools::calcRMSF { dct_parsedArgs }		\\ <tbl_dataRMSF>
#	-	::trajAnalysisTools::calcRMSD { dct_parsedArgs }		\\ <tbl_dataRMSD>
#	-	::trajAnalysisTools::calcDIST { dct_parsedArgs }		\\ <tbl_dataDIST>
#	-	::trajAnalysisTools::calcHEATMAP { dct_parsedArgs }		\\ <tbl_dataHEATMAP>
#	-	::trajAnalysisTools::calcRAMACHANDRAN { dct_parsedArgs }\\ <tbl_dataRAMACHANDRAN>
#	-	::trajAnalysisTools::getCentreOfMass { sel_curSel }		\\ <vec_centreOfMass>
#
##################################
#	Data flow in this package:
#---------------------------------
#	-	function ?args? >> analyse
#		-	args >> callFunction
#			-	parseArguments { rawArgs }
#			-	calcXXXX { dct_parsedArgs }
#			-	openFile { str_outFile }
#			-	CSVprint { file_output tbl_data }
#			-	closeFile { file_outFile }
#			-	stdoutprint { tbl_data }
#

##########################
#	Create the namespace
##########################
namespace eval ::trajAnalysisTools {
	variable lst_default_selTexts "{(all and not water)}"
	variable flg_debug 0
}

##########################
#	Main Function
##########################

proc analyse { { function "help" } { args "" } } { 
	#	We must use 'args' since it is a 'magical argument', 
	#	which accepts one or more terms when used at end.
	if { [ catch { ::trajAnalysisTools::callFunction $function $args } result ] } then {
		puts stdout "$result"
		::trajAnalysisTools::helpMSG
		return -code error
	}
}

##########################
#	Internal Functions
##########################

proc ::trajAnalysisTools::helpMSG {} {
	puts stdout "Usage: analyse function ?-arg var?..."
	puts stdout "  Functions are: rmsd, rmsf, dist, heatmap, ramachandran"
	puts stdout "  Args are:"
	puts stdout "    -mol <molid> (default: top)"
	puts stdout "    -sel \"<selection text>\" (default: \"(all and not water)\" )"
	puts stdout "    -frames <begin:end> or <begin:step:end> or all (default: all)"
	puts stdout "    -o <filename>"
	puts stdout ""
}

proc ::trajAnalysisTools::parseArguments { { rawArgs } } {

	#	Returns a dictionary of parsed arguments, containing:
	#	-	dct_parsedArgs int_molID
	#	-	dct_parsedArgs lst_selTexts
	#	-	dct_parsedArgs str_outFile
	#	-	dct_parsedArgs int_startFrame
	#	-	dct_parsedArgs int_stepFrame
	#	-	dct_parsedArgs int_endFrame

	variable lst_default_selTexts
	variable flg_debug

	#	Create the parsedArgs dictionary
	set dct_parsedArgs [ dict create ]

	#	Check if there are an even number of argument pairs, if not return error.
	set int_nArgs [ llength $rawArgs ]
	if { $int_nArgs % 2 } then {
		return -code error "\[trajAnalysisTools\] Error: odd number of arguments ($int_nArgs)"
	}

	#	Split args into pairs, put into temporary array 'arg'
	foreach { name val } $rawArgs {
		switch -- $name {
			-mol { 
				set arg(molID) $val 
			}
			-sel { 
				lappend arg(selText) $val
			}
			-frames { 
				set arg(frames) $val 
			}
			-o { 
				set arg(o) $val 
			}
			default { 
				return -code error "\[trajAnalysisTools\] Error: Unknown argument: $name $val"
			}
		}
	}

	#	Set defaults if not specified
	if [ info exists arg(molID) ] {
		if [ string is integer $arg(molID) ] {
			dict set dct_parsedArgs int_molID $arg(molID)
		}
	} else {
		dict set dct_parsedArgs int_molID [ molinfo top ]
	}

	if [ info exists arg(selText) ] {
		dict set dct_parsedArgs lst_selTexts $arg(selText)
	} else {
		dict set dct_parsedArgs lst_selTexts $lst_default_selTexts
	}
	
	#	And check if the molecule specified actually exists
	if { [ molinfo index [ dict get $dct_parsedArgs int_molID ] ] == -1 } then {
		return -code error "\[trajAnalysisTools\] Error: Molecule specified does not exist, or no molecule loaded."
	}

	#	If output file was given, pack it in the parsed arguments
	if [ info exists arg(o) ] {
		dict set dct_parsedArgs str_outFile $arg(o)
	}

	#	Set frame selections from arg(frames).
	set int_lastFrame [ expr [ molinfo [ dict get $dct_parsedArgs int_molID ] get numframes ] - 1 ]
	if [ info exists arg(frames) ] {
		set lst_frameSel [ split $arg(frames) : ]
		switch -- [ llength $lst_frameSel ] {
			1 { 
				switch -- $lst_frameSel {
					all {
						set int_startFrame 0
						set int_endFrame $int_lastFrame
					} 
					default {
						return -code error "\[trajAnalysisTools\] Error: bad -frames arg: $arg(frames)"
					} 
				}
			}
			2 { 
				set int_startFrame [ lindex $lst_frameSel 0 ]
				set int_endFrame [ lindex $lst_frameSel 1 ]
			}
			3 { 
				set int_startFrame [ lindex $lst_frameSel 0 ]
				set int_stepFrame [ lindex $lst_frameSel 1 ]
				set int_endFrame [ lindex $lst_frameSel 2 ]
			}
			default { 
				return -code error "\[trajAnalysisTools\] Error: bad -frames arg: $arg(frames)" 
			}
		}
	}

	#	If frame selections were not given, set defaults.
	if { ! [ info exists int_startFrame ] } { set int_startFrame 0 }
	if { ! [ info exists int_stepFrame ] } { set int_stepFrame 1 }
	if { ! [ info exists int_endFrame ] } { set int_endFrame $int_lastFrame }

	switch -- $int_endFrame {
		start - end - last { 
			set int_endFrame $int_lastFrame 
		}
	}
	switch -- $int_startFrame {
		begin - start - first {
			set int_startFrame 0
		}
	}

	#	Catch errors --- account for -ve frame numbers, and iff all framenumbers are sensible push to parsedArgs dictionary. 
	if { 
		[ catch {
			if { $int_startFrame < 0 } {
				set int_startFrame [ expr $int_lastFrame + 1 + $int_startFrame ]
			}
			if { $int_endFrame < 0 } {
				set int_endFrame [ expr $int_lastFrame + 1 + $int_endFrame ]
			}
			if { 
				! ( 
					[ string is integer $int_startFrame ] && \
					( $int_startFrame >= 0 ) && \
					( $int_startFrame <= $int_lastFrame ) && \
					[ string is integer $int_endFrame ] && \
  					( $int_endFrame >= 0 ) && \
  					( $int_endFrame <= $int_lastFrame ) && \
  					( $int_startFrame <= $int_endFrame ) && \
  					[ string is integer $int_stepFrame ] && \
  					( $int_stepFrame > 0 ) 
				) 
			} then {
				error
			} else {
				dict set dct_parsedArgs int_startFrame $int_startFrame
				dict set dct_parsedArgs int_stepFrame $int_stepFrame
				dict set dct_parsedArgs int_endFrame $int_endFrame
			}
		} ]
	} then { 
		return -code error "\[trajAnalysisTools\] Error: bad -frames arg: $arg(frames)"
	}

	#	If in debug mode, print all frame values
	if $flg_debug {
		puts "\[trajAnalysisTools\] Debug: int_startFrame: [ dict get $dct_parsedArgs int_startFrame ]"
		puts "\[trajAnalysisTools\] Debug: int_stepFrame: [ dict get $dct_parsedArgs int_stepFrame ]"
		puts "\[trajAnalysisTools\] Debug: int_endFrame: [ dict get $dct_parsedArgs int_endFrame ]"
	}

	#	Return the entire parsedArgs dictionary.
	return $dct_parsedArgs
}

proc ::trajAnalysisTools::callFunction { function { rawArgs } } {

	#	Parse the arguments
	if { [ catch { parseArguments $rawArgs } dct_parsedArgs ] } then {
		return -code error $dct_parsedArgs
	} 

	#	Check function, and run the calculations
	switch -- $function {
		help { 
			return -code error
		}
		rmsd { 
			set tbl_data [ calcRMSD $dct_parsedArgs ]
		}
		rmsf { 
			set tbl_data [ calcRMSF $dct_parsedArgs ]
		}
		dist {
			set tbl_data [ calcDIST $dct_parsedArgs ]
		}
		heatmap { 
			set tbl_data [ calcHEATMAP $dct_parsedArgs ]
		}
		ramachandran { 
			set tbl_data [ calcRAMACHANDRAN $dct_parsedArgs ]
		}
		default { 
			return -code error "\[trajAnalysisTools\] Error: Unknown function: $function" 
		}
	}

	#	If an output was specified, print to CSV, else print to stdout
	if [ dict exists $dct_parsedArgs str_outFile ] {
		set file_outFile [ openFile [ dict get $dct_parsedArgs str_outFile ] ]
		CSVprint $file_outFile $tbl_data
		closeFile $file_outFile
	} else {
		stdoutprint $tbl_data
	}
}

proc ::trajAnalysisTools::openFile { str_outFile } {
	if { [ catch { open $str_outFile w } file_outFile ] } then {
		return -code error "\[trajAnalysisTools\] Error: Could not open [ dict get $dct_parsedArgs str_outFile ] for writing"
	} else {
		return $file_outFile
	}
}

proc ::trajAnalysisTools::closeFile { file_outFile } {
	#	Flush, and then close file channel
	flush $file_outFile
	close $file_outFile
}

proc ::trajAnalysisTools::CSVprint { file_outFile tbl_data } {
	#	Iterate through each list record in the table
	foreach lst_record $tbl_data {
		set str_record ""
		#	Change each record to a comma separated string
		foreach str_item $lst_record {
			append str_record $str_item ","
		}
		#	Trim trailing comma 
		set str_record [ string trimright $str_record "," ]
		#	Print record to file
		puts $file_outFile $str_record
	}
}

proc ::trajAnalysisTools::stdoutprint { tbl_data } {
	foreach lst_record $tbl_data {
		puts stdout $lst_record
	}
}

proc ::trajAnalysisTools::calcRMSF { dct_parsedArgs } {
	#	Measure RMSF can cope with:
	#	-	ONE selText
	#	-	framerange
	#	-	molID

	#	Unpack variables from dct_parsedArgs
	set int_molID [ dict get $dct_parsedArgs int_molID ]
	set lst_selTexts [ dict get $dct_parsedArgs lst_selTexts ]
	set int_startFrame [ dict get $dct_parsedArgs int_startFrame ]
	set int_stepFrame [ dict get $dct_parsedArgs int_stepFrame ]
	set int_endFrame [ dict get $dct_parsedArgs int_endFrame ]

	#	If more than one selection was specified, this is invalid for this procedure.
	if { [ llength $lst_selTexts ] > 1 } then {
		return -code error "\[trajAnalysisTools\] Error: RMSF can only calculate for one selection. Your selection was $lst_selTexts."
	}

	#	Select all Cα in the selection, get the atom indices of these.
	set sel_allCa [ atomselect $int_molID "[ lindex $lst_selTexts 0 ] and name CA" ]
	set lst_indexCa [ $sel_allCa get index ]

	#	Create a table (list of lists) 'tbl_dataRMSF', and set the first entry (column headings)
	set tbl_dataRMSF [ list [ list "resid" "resname" "RMSF-Residue" "RMSF-Residue-noH" "RMSF-Ca" ] ]

	#	Progress checks
	set int_lastProgressPercent 0
	set int_posIndexCa 0
	set int_lenIndexCa [ llength $lst_indexCa ]
	#	Iterate through each Cα
	#		Select the entire residue
	#		Determine the residue number, residue name, RMSFs
	#		Append to the table an entry of these variables
	#		Print progress
	foreach C $lst_indexCa {
		incr int_posIndexCa
		set sel_curRes [ atomselect $int_molID "same residue as index $C" ]
		set sel_curResnoh [ atomselect $int_molID "same residue as index $C and noh"]
		set sel_curCa [ atomselect $int_molID "index $C" ]
		set int_resNum [ $sel_curCa get resid ]
		set str_resName [ $sel_curCa get resname ]
		set int_resRMSF [ vecmean [ measure rmsf $sel_curRes first $int_startFrame last $int_endFrame step $int_stepFrame ] ]
		set int_resnoHRMSF [ vecmean [ measure rmsf $sel_curResnoh first $int_startFrame last $int_endFrame step $int_stepFrame ] ]
		set int_CaRMSF [ vecmean [ measure rmsf $sel_curCa first $int_startFrame last $int_endFrame step $int_stepFrame ] ]
	   	lappend tbl_dataRMSF [ list $int_resNum $str_resName $int_resRMSF $int_resnoHRMSF $int_CaRMSF ]
	   	#	Print progress
		if { [ expr $int_posIndexCa * 100 / $int_lenIndexCa ] > $int_lastProgressPercent } then {
			puts "\[Info\] trajAnalysisTools: $int_lastProgressPercent\%: RMSF analysis completed for $int_posIndexCa/$int_lenIndexCa residues"
			set int_lastProgressPercent [ expr $int_posIndexCa * 100 / $int_lenIndexCa ]
		}
	}

	#	Return the table
	return $tbl_dataRMSF
}

proc ::trajAnalysisTools::calcRMSD { dct_parsedArgs } {
	#	Measure RMSD can cope with:
	#	-	selTexts
	#	-	framerange
	#	-	molID

	#	Unpack variables from dct_parsedArgs
	set int_molID [ dict get $dct_parsedArgs int_molID ]
	set lst_selTexts [ dict get $dct_parsedArgs lst_selTexts ]
	set int_startFrame [ dict get $dct_parsedArgs int_startFrame ]
	set int_stepFrame [ dict get $dct_parsedArgs int_stepFrame ]
	set int_endFrame [ dict get $dct_parsedArgs int_endFrame ]

	#	Initialise headings list, and tbl_dataRMSD
	set tbl_dataRMSD [ list ]
	set lst_dataHeadings [ list "frame" ]

	#	Unpack lst_selTexts into full lists of all selections (current frames, reference frames)
	foreach str_selText $lst_selTexts {
		lappend lst_dataHeadings "RMSD-\[$str_selText\]-massw" 
		lappend lst_dataHeadings "RMSD-\[$str_selText\]-noH-massw" 
		lappend lst_dataHeadings "RMSD-\[$str_selText\]-Ca"
		lappend lst_allSels [ atomselect $int_molID "$str_selText" ]
		lappend lst_allSels [ atomselect $int_molID "$str_selText" frame $int_startFrame ]
		lappend lst_allSels [ atomselect $int_molID "$str_selText and noh" ]
		lappend lst_allSels [ atomselect $int_molID "$str_selText and noh" frame $int_startFrame ]
		lappend lst_allSels [ atomselect $int_molID "$str_selText and name CA" ]
		lappend lst_allSels [ atomselect $int_molID "$str_selText and name CA" frame $int_startFrame ]
	}

	#	Append headings list to tbl_dataRMSD
	lappend tbl_dataRMSD $lst_dataHeadings

	#	Progress checks
	set int_lastProgressPercent 0
	#	Iterate through each frame
	#		Initialise the data list for the current frame
	#		Iterate through each selection
	#			Select the frame
	#			Determine the RMSD from the reference frame
	#			Append to the current frame list
	#		Append the data list for the current frame to the data table
	for {set int_curFrame $int_startFrame} {$int_curFrame <= $int_endFrame} {incr int_curFrame $int_stepFrame} {
		set lst_dataCurFrame [ list $int_curFrame ]
		foreach { sel_curSel_curFrame sel_curSel_refFrame } $lst_allSels {
			$sel_curSel_curFrame frame $int_curFrame
			lappend lst_dataCurFrame [ measure rmsd $sel_curSel_refFrame $sel_curSel_curFrame weight mass ]
		}
		lappend tbl_dataRMSD $lst_dataCurFrame
		#	Print progress
		if { [ expr $int_curFrame * 100 / $int_endFrame ] > $int_lastProgressPercent } then {
			puts "\[Info\] trajAnalysisTools: $int_lastProgressPercent\%: RMSD for frame $int_curFrame/$int_endFrame completed"
			set int_lastProgressPercent [ expr $int_curFrame * 100 / $int_endFrame ]
		}
	}
	
	#	Return the table
	return $tbl_dataRMSD
}

proc ::trajAnalysisTools::calcDIST { dct_parsedArgs } {
	#	Unpack variables from dct_parsedArgs
	set int_molID [ dict get $dct_parsedArgs int_molID ]
	set lst_selTexts [ dict get $dct_parsedArgs lst_selTexts ]
	set int_startFrame [ dict get $dct_parsedArgs int_startFrame ]
	set int_stepFrame [ dict get $dct_parsedArgs int_stepFrame ]
	set int_endFrame [ dict get $dct_parsedArgs int_endFrame ]

	#	Initialise headings list, and tbl_dataRMSD
	set tbl_dataDIST [ list ]
	set lst_dataHeadings [ list "frame" ]

	#	Check that number of selections = 2, if not, quit.
	if { [ llength $lst_selTexts ] != 2 } then {
		return -code error "\[trajAnalysisTools\] Error: Distance can only be calculated between two selections. Your selection was $lst_selTexts."
	}

	#	Finish off headings list and append to tbl_dataRMSD
	lappend lst_dataHeadings "\([ lindex $lst_selTexts 0 ]\) to \([ lindex $lst_selTexts 1 ]\)" 
	lappend tbl_dataDIST $lst_dataHeadings

	#	Unpack lst_selTexts into full lists of all selections (current frames, reference frames)
	set sel_sel1 [ atomselect $int_molID "[ lindex $lst_selTexts 0 ]" ]
	set sel_sel2 [ atomselect $int_molID "[ lindex $lst_selTexts 1 ]" ]
	
	#	Progress checks
	set int_lastProgressPercent 0
	#	Iterate through each frame
	#		Initialise the data list for the current frame
	#		Calculate the centre of mass displacement vectors for each selection
	#		Calculate the vector distance between each selection (vecsub $v2 $v1)
	#		Calculate the magnitude of this distance vector (veclength $v)
	#		Append the data list for the current frame to the data table
	for {set int_curFrame $int_startFrame} {$int_curFrame <= $int_endFrame} {incr int_curFrame $int_stepFrame} {
		set lst_dataCurFrame [ list $int_curFrame ]
		set sel_sel1_curFrame $sel_sel1 frame $int_curFrame
		set sel_sel2_curFrame $sel_sel2 frame $int_curFrame
		lappend lst_dataCurFrame [ veclength [ vecsub [ getCentreOfMass $sel_sel2_curFrame ] [ getCentreOfMass $sel_sel1_curFrame ] ] ]
		lappend tbl_dataDIST $lst_dataCurFrame
		#	Print progress
		if { [ expr $int_curFrame * 100 / $int_endFrame ] > $int_lastProgressPercent } then {
			puts "\[Info\] trajAnalysisTools: $int_lastProgressPercent\%: RMSD for frame $int_curFrame/$int_endFrame completed"
			set int_lastProgressPercent [ expr $int_curFrame * 100 / $int_endFrame ]
		}
	}
	
	#	Return the table
	return $tbl_dataDIST
}

proc ::trajAnalysisTools::calcHEATMAP { dct_parsedArgs } {

	#	Unpack variables from dct_parsedArgs
	set int_molID [ dict get $dct_parsedArgs int_molID ]
	set lst_selTexts [ dict get $dct_parsedArgs lst_selTexts ]
	set int_startFrame [ dict get $dct_parsedArgs int_startFrame ]
	set int_stepFrame [ dict get $dct_parsedArgs int_stepFrame ]
	set int_endFrame [ dict get $dct_parsedArgs int_endFrame ]

	#	If more than one selection was specified, this is invalid for this procedure.
	if { [ llength $lst_selTexts ] > 1 } then {
		return -code error "\[trajAnalysisTools\] Error: HEATMAP can only calculate for one selection. Your selection was $lst_selTexts."
	}

	#	Initialise headings list, and tbl_dataHEATMAP
	set tbl_dataHEATMAP [ list ]
	set lst_dataHeadings [ list "frame" "resid" "resname" ]
	lappend lst_dataHeadings "RMSD-\[[ lindex $lst_selTexts 0 ]\]-massw" 
	lappend lst_dataHeadings "RMSD-\[[ lindex $lst_selTexts 0 ]\]-noH-massw" 
	lappend lst_dataHeadings "RMSD-\[[ lindex $lst_selTexts 0 ]\]-Ca"
	#	Append headings list to tbl_dataHEATMAP
	lappend tbl_dataHEATMAP $lst_dataHeadings

	#	Select all Cα in the selection, get the atom indices of these.
	set sel_allCa [ atomselect $int_molID "[ lindex $lst_selTexts 0 ] and name CA" ]
	set lst_indexCa [ $sel_allCa get index ]

	#	Progress checks
	set int_lastProgressPercent 0
	set int_posIndexCa 0
	set int_lenIndexCa [ llength $lst_indexCa ]
	#	Iterate through each Cα
	#		Select the entire residue
	#		Determine the residue number, residue name
	#		For each frame in the framerange, 
	#		Append to the table an entry of these variables
	#		Print progress
	foreach C $lst_indexCa {
		incr int_posIndexCa
		set sel_curCa_curFrame [ atomselect $int_molID "index $C" ]
		set sel_curCa_refFrame [ atomselect $int_molID "index $C" frame $int_startFrame ]
		set int_resNum [ $sel_curCa_curFrame get resid ]
		set str_resName [ $sel_curCa_curFrame get resname ]
		set sel_curRes_curFrame [ atomselect $int_molID "same residue as index $C" ]
		set sel_curRes_refFrame [ atomselect $int_molID "same residue as index $C" frame $int_startFrame ]
		set sel_curResnoh_curFrame [ atomselect $int_molID "same residue as index $C and noh" ]
		set sel_curResnoh_refFrame [ atomselect $int_molID "same residue as index $C and noh" frame $int_startFrame ]
		
		for {set int_curFrame $int_startFrame} {$int_curFrame <= $int_endFrame} {incr int_curFrame $int_stepFrame} {
			set lst_dataCurFrame [ list $int_curFrame $int_resNum $str_resName ]
			$sel_curRes_curFrame frame $int_curFrame
			lappend lst_dataCurFrame [ measure rmsd $sel_curRes_refFrame $sel_curRes_curFrame weight mass ]
			$sel_curResnoh_curFrame frame $int_curFrame
			lappend lst_dataCurFrame [ measure rmsd $sel_curResnoh_refFrame $sel_curResnoh_curFrame weight mass ]
			$sel_curCa_curFrame frame $int_curFrame
			lappend lst_dataCurFrame [ measure rmsd $sel_curCa_refFrame $sel_curCa_curFrame weight mass ]

			lappend tbl_dataHEATMAP $lst_dataCurFrame
		}

	   	#	Print progress
		if { [ expr $int_posIndexCa * 100 / $int_lenIndexCa ] > $int_lastProgressPercent } then {
			puts "\[Info\] trajAnalysisTools: $int_lastProgressPercent\%: HEATMAP analysis completed for $int_posIndexCa/$int_lenIndexCa residues"
			set int_lastProgressPercent [ expr $int_posIndexCa * 100 / $int_lenIndexCa ]
		}
	}

	#	Return the table
	return $tbl_dataHEATMAP
}

proc ::trajAnalysisTools::calcRAMACHANDRAN { dct_parsedArgs } {

	#	Unpack variables from dct_parsedArgs
	set int_molID [ dict get $dct_parsedArgs int_molID ]
	set lst_selTexts [ dict get $dct_parsedArgs lst_selTexts ]
	set int_startFrame [ dict get $dct_parsedArgs int_startFrame ]
	set int_stepFrame [ dict get $dct_parsedArgs int_stepFrame ]
	set int_endFrame [ dict get $dct_parsedArgs int_endFrame ]

	#	If more than one selection was specified, this is invalid for this procedure.
	if { [ llength $lst_selTexts ] > 1 } then {
		return -code error "\[trajAnalysisTools\] Error: RAMACHANDRAN can only calculate for one selection. Your selection was $lst_selTexts."
	}

	#	Initialise headings list, and tbl_dataRAMACHANDRAN
	set tbl_dataRAMACHANDRAN [ list ]
	set lst_dataHeadings [ list "frame" "resid" "resname" ]
	lappend lst_dataHeadings "phi" 
	lappend lst_dataHeadings "psi" 
	#	Append headings list to tbl_dataRAMACHANDRAN
	lappend tbl_dataRAMACHANDRAN $lst_dataHeadings

	#	Select all Cα in the selection, get the atom indices of these.
	set sel_allCa [ atomselect $int_molID "[ lindex $lst_selTexts 0 ] and name CA" ]
	set lst_indexCa [ $sel_allCa get index ]

	#	Progress checks
	set int_lastProgressPercent 0
	set int_posIndexCa 0
	set int_lenIndexCa [ llength $lst_indexCa ]
	#	Iterate through each Cα
	#		Determine the residue number, residue name
	#		For each frame in the framerange, determine phi & psi
	#		Append to the table an entry of these variables
	#		Print progress
	foreach C $lst_indexCa {
		incr int_posIndexCa
		set sel_curCa_curFrame [ atomselect $int_molID "index $C" ]
		set int_resNum [ $sel_curCa_curFrame get resid ]
		set str_resName [ $sel_curCa_curFrame get resname ]
		for {set int_curFrame $int_startFrame} {$int_curFrame <= $int_endFrame} {incr int_curFrame $int_stepFrame} {
			set lst_dataCurFrame [ list $int_curFrame $int_resNum $str_resName ]
			$sel_curCa_curFrame frame $int_curFrame
			lappend lst_dataCurFrame [ $sel_curCa_curFrame get phi ] [ $sel_curCa_curFrame get psi ]
			lappend tbl_dataRAMACHANDRAN $lst_dataCurFrame
		}

	   	#	Print progress
		if { [ expr $int_posIndexCa * 100 / $int_lenIndexCa ] > $int_lastProgressPercent } then {
			puts "\[Info\] trajAnalysisTools: $int_lastProgressPercent\%: RAMACHANDRAN analysis completed for $int_posIndexCa/$int_lenIndexCa residues"
			set int_lastProgressPercent [ expr $int_posIndexCa * 100 / $int_lenIndexCa ]
		}
	}

	#	Return the table
	return $tbl_dataRAMACHANDRAN
}

proc ::trajAnalysisTools::getCentreOfMass { sel_curSel } {
	
	#	Make sure that selection isn't null:
	if { [ $sel_curSel num ] <= 0 } {
		return -code error "\[trajAnalysisTools\] Error: Can't calculate centre of mass for an empty selection."
	}

	#	Create vector, total mass
	set vec_centreOfMass [ veczero ]
	set int_totalMass 0

	#	For each atom in the selection:
	#		Get the coordinates and masses
	#		Sum the mass-weighted displacement vector
	foreach vec_coordAtom [ $sel_curSel get { x y z } ] int_massAtom [ $sel_curSel get mass ] {
		set int_totalMass [ expr $int_totalMass + $int_massAtom ]
		set vec_centreOfMass [ vecadd $vec_centreOfMass [ vecscale $int_massAtom $vec_coordAtom ] ]
	}

	#	Finally, divide by the total mass (if non-zero) and return
	if { $int_totalMass == 0 } then {
		return -code error "\[trajAnalysisTools\] Error: Can't calculate centre of mass. Selection is massless."
	} else {
		return [ vecscale [ expr 1.0 / $int_totalMass ] $vec_centreOfMass ]
	}

}
