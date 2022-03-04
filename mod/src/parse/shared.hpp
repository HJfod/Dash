#pragma once

#include <Geode.hpp>
#include "Managed.hpp"

USE_GEODE_NAMESPACE();

Result<ccColor3B> parseColor(std::string str);
Result<CCRect> parseRect(std::string const& str);
