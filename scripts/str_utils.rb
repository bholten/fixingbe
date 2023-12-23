module StrUtils
  module_function

  def to_snake_case(str)
    str.downcase.gsub(/\s+/, '_').gsub(/[^\w_]/, '')
  end
end
