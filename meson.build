project(
  'beyond-rpa',
  'fortran',
  version : '1.0',
  default_options : [
    'warning_level=3',
    'buildtype=plain',
    'backend=ninja',
  ]
)

if meson.backend() != 'ninja'
  error('This project only supports the Ninja backend.')
endif

subdir('src')

# Compiler command string constructed solely for launcher metadata
# logging in bin/run
fc = meson.get_compiler('fortran')
compiler_cmd = fc.cmd_array()[0]
foreach arg : get_option('fortran_args')
  compiler_cmd += ' ' + arg
endforeach

# Linker flags (including MKL, BLAS, and LAPACK dependencies) are configured
# per profile in .meson/profiles/*.ini
a_exe = executable('a', sources)

python = import('python').find_installation('python3')
post_build = files('.meson/scripts/post_build.py')
bin_dir = meson.project_source_root() / 'bin'

launcher_cmd = meson.get_external_property('launcher', '')
intel_caf_config = meson.get_external_property('intel_coarray_config', '')

post_build_target = custom_target('post_build_tasks',
  input : a_exe,
  output : 'post_build.done',
  command : [
    python, post_build, '@INPUT@', bin_dir,
    launcher_cmd, intel_caf_config, compiler_cmd,
    '@OUTPUT@',
  ],
  build_by_default : true,
)

test('basis',
  python,
  args: [
    '-m', 'pytest',
    '-v', '-s',
    meson.project_source_root() / 'tests' / 'basis' / 'test_basis.py'
  ],
  depends: post_build_target,
  timeout: 600,
  workdir: meson.project_source_root()
)
