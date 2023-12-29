require 'csv'
require 'nokogiri'

require_relative 'furrycat_page'

DATA_RAW_DIR = '../data/raw/furrycat'
DATA_CLEAN_DIR = '../data/clean/furrycat/'

TEMPLATES_FILENAME = DATA_CLEAN_DIR + 'templates.csv'
CREATURES_FILENAME = DATA_CLEAN_DIR + 'creatures.csv'
EXPERIMENTS_FILENAME = DATA_CLEAN_DIR + 'experiments.csv'
SAMPLES_FILENAME = DATA_CLEAN_DIR + 'samples.csv'

def file_empty?(filename)
  !File.exist?(filename)
end

def parse_files
  CSV.open(TEMPLATES_FILENAME, 'w') do |templates_csv|
    CSV.open(CREATURES_FILENAME, 'w') do |creatures_csv|
      CSV.open(EXPERIMENTS_FILENAME, 'w') do |experiments_csv|
        CSV.open(SAMPLES_FILENAME, 'w') do |samples_csv|

          Dir.glob("#{DATA_RAW_DIR}/*.html").each.with_index do |file, index|
            creature_data = File.open(file) { |f| Nokogiri::HTML(f) }

            begin
              furrycat_page = FurryCatPage.from_html(creature_data)
            rescue => error
              puts "Error in file #{file} #{error.message}"
              break
            end

            if index == 0
              creatures_csv << furrycat_page.creature.keys
              experiments_csv << furrycat_page.experiments.first.keys
              templates_csv << furrycat_page.template.keys
              samples_csv << furrycat_page.samples.first.keys
            end

            puts "Adding creature: #{furrycat_page.creature[:serial]}"

            creatures_csv << furrycat_page.creature.values
            templates_csv << furrycat_page.template.values

            furrycat_page.experiments.each do |experiment|
              experiments_csv << experiment.values_at(*furrycat_page.experiments.first.keys)
            end

            furrycat_page.samples.each do |sample|
              samples_csv << sample.values_at(*furrycat_page.samples.first.keys)
            end
          end
        end
      end
    end
  end
end

begin
  parse_files
rescue => exception
  puts exception
end
