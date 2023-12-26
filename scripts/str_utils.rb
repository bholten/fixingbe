module StrUtils
  module_function

  def to_snake_case(str)
    str.downcase.gsub(/\s+/, '_').gsub(/[^\w_]/, '')
  end

  def is_light_armor(str)
    if str.strip.casecmp?('light') then 1 else 0 end
  end
end
