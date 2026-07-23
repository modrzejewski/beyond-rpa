project('beyond-rpa', 'fortran', version : '1.0', default_options : ['warning_level=3'])

subdir('src')

fc = meson.get_compiler('fortran')
compiler_cmd = fc.cmd_array()[0]
foreach arg : get_option('fortran_args')
  compiler_cmd += ' ' + arg
endforeach

a_exe = executable('a', sources)

python = import('python').find_installation('python3')
post_build = files('.meson/scripts/post_build.py')
bin_dir = meson.project_source_root() / 'bin'

launcher_cmd = meson.get_external_property('launcher', '')
intel_caf_config = meson.get_external_property('intel_coarray_config', '')

custom_target('post_build_tasks',
  input : a_exe,
  output : 'post_build.done',
  command : [
    python, post_build, '@INPUT@', bin_dir,
    launcher_cmd, intel_caf_config, compiler_cmd,
    '@OUTPUT@',
  ],
  build_by_default : true,
)
