include_guard(GLOBAL)
after_project_guard()


include_project_module(dependencies/Ccache)
use_ccache_if_enabled_for(CXX)
