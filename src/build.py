#!/usr/bin/env python3
#
# Script for compilation and preprocessing of Fortran source code files
# To edit the source code list modify SourceCode.py
#
from subprocess import Popen
import datetime
from datetime import timezone
import argparse
from os import path
import os
import shutil
import sys
import LauncherScript
import CodePreprocess
from SourceCode import AllModules, MinimumModules, PreprocessList, FileList

RootDir = path.dirname(path.realpath(__file__))
OptionsDir = path.join(RootDir, "CompilerFlags")
AvailableFlags = []
AvailableFlags.append("default")
for f in os.listdir(OptionsDir):
    if f != "default":
        AvailableFlags.append(f)
        
parser = argparse.ArgumentParser()
parser.add_argument("CompilerOptions", nargs="?", default="default", choices=AvailableFlags,
                    help="Name of compiler options file located in ./CompilerFlags/")
parser.add_argument("-np", help="Number of concurrent compiler processes", type=int, default=8)
parser.add_argument("--clean", action="store_true", help="Remove all .obj and .mod files")
parser.add_argument("--enable", type=str, default=None, help="List of optional modules to enable for compilation")
args = parser.parse_args()
CompilerOptions = args.CompilerOptions
nproc = args.np
DefaultOptionsDir = path.join(OptionsDir, "default")
if not path.exists(DefaultOptionsDir):
    os.makedirs(DefaultOptionsDir)
ObjDir = path.join(RootDir, "obj")
if not path.exists(ObjDir):
   os.makedirs(ObjDir)
PP_SrcDir = path.join(RootDir, "preprocessed")
if not path.exists(PP_SrcDir):
    os.makedirs(PP_SrcDir)
ExeFile = path.realpath(path.join(RootDir, "../bin/a"))
ExeDir = path.split(ExeFile)[0]
if not path.exists(ExeDir):
   os.makedirs(ExeDir)
RunFile = path.join(ExeDir, "run")
CompilerDefinition = path.join(OptionsDir, CompilerOptions, "compiler")
LinkerDefinition = path.join(OptionsDir, CompilerOptions, "linker")
LauncherDefinition = path.join(OptionsDir, CompilerOptions, "launcher")
IntelCAFConfigFile = path.join(OptionsDir, CompilerOptions, "intel-coarray-config")
DefaultCompiler = path.join(OptionsDir, "default", "compiler")
DefaultLinker = path.join(OptionsDir, "default", "linker")
DefaultLauncher = path.join(OptionsDir, "default", "launcher")
DefaultModules = path.join(OptionsDir, "default", "modules")
DefaultIntelCAFConfigFile = path.join(OptionsDir, "default", "intel-coarray-config")
CompilerCmd = open(CompilerDefinition).readline().rstrip()
LinkerCmd = open(LinkerDefinition).readline().rstrip()
LauncherCmd = open(LauncherDefinition).readline().rstrip()
if path.exists(IntelCAFConfigFile):
    IntelCAFConfig = open(IntelCAFConfigFile).readline().rstrip()
else:
    IntelCAFConfig = ""

CompiledModules = MinimumModules
if args.enable is not None:
    if args.enable.strip() != "":
        module_list = args.enable.split(",")
        for m in module_list:
            if m in AllModules:
                CompiledModules.add(m.upper().strip())
            else:
                print("Module {} is undefined".format(m))
                sys.exit(1)
else:
    if path.exists(DefaultModules):
        f = open(DefaultModules, "r")
        s = f.read()
        f.close()
        module_list = s.split(",")
        for m in module_list:
            CompiledModules.add(m.strip().upper())
f = open(DefaultModules, "w")
f.write(",".join(sorted(CompiledModules)) + "\n")
f.close()

datetime_start = datetime.datetime.now(timezone.utc)

def move_files(BaseName, DstDir):
    ObjFileFrom = BaseName + ".o"
    ObjFileTo = path.join(DstDir, BaseName + ".o")
    if path.exists(ObjFileFrom):
        shutil.move(ObjFileFrom, ObjFileTo)
    ModFileFrom = BaseName + ".mod"
    ModFileTo = path.join(DstDir, BaseName + ".mod")
    if path.exists(ModFileFrom):
        shutil.move(ModFileFrom, ModFileTo)

def test_recomp(SrcFile):
    SrcCreated = path.getmtime(SrcFile)
    BaseName, Ext = path.splitext(path.split(SrcFile)[1])
    ObjFile = path.join(ObjDir, BaseName + ".o")
    if path.exists(ObjFile):
        ObjCreated = path.getmtime(ObjFile)
        return SrcCreated >= ObjCreated
    else:
        return True

def remove_files(RelSrcPath):
    BaseName, Ext = path.splitext(path.split(RelSrcPath)[1])
    remove_list = []
    remove_list.append(path.join(ObjDir, BaseName + ".o"))
    remove_list.append(path.join(ObjDir, BaseName + ".mod"))
    remove_list.append(path.join(ObjDir, BaseName.lower() + ".mod"))
    for p in remove_list:
        if path.exists(p):
            os.remove(p)

def PP_FileName(FilePath):
    d, f = path.split(FilePath)
    return path.join(PP_SrcDir, f)
            

if args.clean:
    for module, batch in FileList:
        if module not in CompiledModules:
            continue
        for File in batch:
            remove_files(File)
    sys.exit()

if not path.exists(CompilerDefinition) or not path.exists(LinkerDefinition):
    print("Cannot find compiler and linker definitions. Aborting compilation.")
    sys.exit(1)
#
# Preprocessing of source code files
#
PP_Paths = set([])
for m in AllModules:
    if m in PreprocessList:
        BeginString = "!@{}".format(m)
        EndString = "!@END {}".format(m)
        for p in PreprocessList[m]:
            AbsSrcPath = path.join(RootDir, p)
            PP_Paths.add(AbsSrcPath)
            PP_AbsSrcPath = PP_FileName(AbsSrcPath)
            print("copying " + AbsSrcPath + " to " + PP_AbsSrcPath)
            shutil.copyfile(AbsSrcPath, PP_AbsSrcPath)
            if m in CompiledModules:
                CodePreprocess.SwitchOn(PP_AbsSrcPath, BeginString, EndString)
            else:
                CodePreprocess.SwitchOff(PP_AbsSrcPath, BeginString, EndString)
#
# The current compiler options will be the default options
# for the next compilation. (The default options are used
# if the corresponding positional argument is skipped.)
#
if args.CompilerOptions != "default":
    shutil.copyfile(CompilerDefinition, DefaultCompiler)
    shutil.copyfile(LinkerDefinition, DefaultLinker)
    shutil.copyfile(LauncherDefinition, DefaultLauncher)
    open(DefaultIntelCAFConfigFile, "w").write(IntelCAFConfig)

ModifiedGroups = set()
RecompCascade = False
for module, batch in FileList:
    if module not in CompiledModules:
        continue
    processes = []
    outfiles = []
    nstarted = 0
    ncompiled = 0
    nunchanged = 0
    while ncompiled+nunchanged < len(batch):
        if nstarted - ncompiled < nproc and nunchanged+nstarted < len(batch):
            RelSrcPath = batch[nunchanged+nstarted]
            AbsSrcPath = path.join(RootDir, RelSrcPath)
            AbsSrcDir = path.split(AbsSrcPath)[0]
            ModifiedSource = test_recomp(AbsSrcPath)
            if ModifiedSource:
                ModifiedGroups.add(AbsSrcDir)
            if ModifiedSource or (RecompCascade and AbsSrcDir in ModifiedGroups):
                BaseName, Ext = path.splitext(path.split(AbsSrcPath)[1])
                Display = CompilerCmd + " " + path.split(AbsSrcPath)[1]
                if AbsSrcPath in PP_Paths:
                    Command = CompilerCmd + " " + PP_FileName(AbsSrcPath)
                else:
                    Command = CompilerCmd + " " + AbsSrcPath
                print(Display)
                processes.append(Popen(Command, shell=True))
                outfiles.append(BaseName)
                nstarted += 1
            else:
                nunchanged += 1
        else:
            error_code = processes[ncompiled].wait()
            if error_code != 0:
                sys.exit(error_code)
            move_files(outfiles[ncompiled], ObjDir)
            #
            # When the camel case convention is used for module names,
            # the compiler may produce lower case names for the mod files
            #
            move_files(outfiles[ncompiled].lower(), ObjDir)
            ncompiled += 1
    if ncompiled > 0:
        RecompCascade = True
#
# Make the list of files to link
#
ObjList = []
for module, b in FileList:
    if module not in CompiledModules:
        continue
    for f in b:
        RelSrcPath = f
        AbsSrcPath = path.join(RootDir, RelSrcPath)
        if AbsSrcPath in PP_Paths:
            g = PP_FileName(AbsSrcPath)
        else:
            g = AbsSrcPath
        BaseName, Ext = path.splitext(path.split(g)[1])
        ObjFile = path.join(ObjDir, BaseName + ".o")
        ObjList.append(ObjFile)
LinkerName, LinkerOptions = LinkerCmd.split(maxsplit=1)
Command = LinkerName + " " + " ".join(ObjList) + " " + LinkerOptions + " " + ExeFile
Display = LinkerName + " " + "[object files in {}]".format(ObjDir) + " " + LinkerOptions + " " + ExeFile
print(Display)
p = Popen(Command, shell=True)
error_code = p.wait()
if error_code != 0:
    sys.exit(error_code)

datetime_finish = datetime.datetime.now(timezone.utc)
td = datetime_finish - datetime_start
dateformat = "%a %b %d %H:%M:%S UTC %Y"
datestr_finish = datetime_finish.strftime(dateformat)

LauncherScript.make_runscript(RunFile, CompilerCmd, datestr_finish, CAFLauncher=LauncherCmd, IntelCAFConfig=IntelCAFConfig)

print("% Compilation completed successfully at {}".format(datestr_finish))
print("% Employed {} concurrent processes".format(nproc))
print("% Total wall clock time [s]: {walltime:.1f}".format(walltime=td.total_seconds()))
