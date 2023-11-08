#!/usr/bin/env python3
#
# Enable/disable annotated sections of a Fortran file. Example:
# SwitchOn("./driver.f90", "!@COUPLED-CLUSTERS", "!@END COUPLED-CLUSTERS")
#
# Coded by Marcin Modrzejewski (Charles Univ, October 2018)
#

COMMENT_STRING = "! -- auto switch -- "

def AnalyzeSections(BeginString, EndString, CommentString, Lines):
    Comment = CommentString.upper()
    String0 = BeginString.strip().upper()
    String1 = EndString.strip().upper()
    Inner = set()
    Outer = set()
    InsideSection = False
    IsEnabled = False
    for i in range(len(Lines)):
        s = Lines[i].lstrip().upper()
        if not InsideSection:
            if s.startswith(String0):
                InsideSection = True
            else:
                Outer.add(i)
        else:
            if s.startswith(String1):
                InsideSection = False
            else:
                Inner.add(i)
                if not s.startswith(Comment):
                    #
                    # If there's at least a single line of code not commented out
                    # in the specified section, change status to enabled.
                    #
                    IsEnabled = True
    return IsEnabled, Inner, Outer


def DisableSections(Inner, Outer, OldLines, CommentChar):
    NewLines = []
    for i in range(len(OldLines)):
        if i in Inner:
            NewLines.append(CommentChar + OldLines[i])
        else:
            NewLines.append(OldLines[i])
    return NewLines
    

def EnableSections(Inner, Outer, OldLines, CommentChar):
    NewLines = []
    for i in range(len(OldLines)):
        if i in Inner:
            NewLine = "".join(OldLines[i].split(CommentChar))
            NewLines.append(NewLine)
        else:
            NewLines.append(OldLines[i])
    return NewLines
    

def SwitchOn(FileName, BeginString, EndString):
    f = open(FileName, "r")
    OldLines = f.readlines()
    f.close()
    IsEnabled, Inner, Outer = AnalyzeSections(BeginString, EndString, COMMENT_STRING, OldLines)
    if not IsEnabled:
        NewLines = EnableSections(Inner, Outer, OldLines, COMMENT_STRING)
        f = open(FileName, "w")
        for s in NewLines:
            f.write(s)
        f.close()

def SwitchOff(FileName, BeginString, EndString):
    f = open(FileName, "r")
    OldLines = f.readlines()
    f.close()
    IsEnabled, Inner, Outer = AnalyzeSections(BeginString, EndString, COMMENT_STRING, OldLines)
    if IsEnabled:
        NewLines = DisableSections(Inner, Outer, OldLines, COMMENT_STRING)
        f = open(FileName, "w")
        for s in NewLines:
            f.write(s)
        f.close()


