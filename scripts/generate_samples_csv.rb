require 'csv'
require 'nokogiri'

require_relative 'furrycat_page'

DATA_RAW_DIR = '../data/raw/furrycat'
DATA_CLEAN_DIR = '../data/clean/furrycat/'

ASSEMBLIES_FILENAME = DATA_CLEAN_DIR + 'assemblies.csv'
CREATURES_FILENAME = DATA_CLEAN_DIR + 'creature.csv'
FINAL_EXPERIMENTS_FILENAME = DATA_CLEAN_DIR + 'final_experiment.csv'
EXPERIMENTS_FILENAME = DATA_CLEAN_DIR + 'experiments.csv'
SAMPLES_FILENAME = DATA_CLEAN_DIR + 'samples.csv'

def file_empty?(filename)
  !File.exist?(filename)
end

def parse_files
  CSV.open(ASSEMBLIES_FILENAME, 'w') do |assemblies_csv|
    CSV.open(CREATURES_FILENAME, 'w') do |creatures_csv|
      CSV.open(FINAL_EXPERIMENTS_FILENAME, 'w') do |final_experiments_csv|
        CSV.open(EXPERIMENTS_FILENAME, 'w') do |experiments_csv|
          CSV.open(SAMPLES_FILENAME, 'w') do |samples_csv|

            Dir.glob("#{DATA_RAW_DIR}/*.html").each.with_index do |file, index|
              creature_data = File.open(file) { |f| Nokogiri::HTML(f) }

              begin
                furrycat_page = FurryCatPage.from_html(creature_data)
              rescue => error
                puts "Error in file #{file}"
                break
              end

              if index == 0
                creatures_csv << furrycat_page.creature.keys
                final_experiments_csv << furrycat_page.final_experiment.keys
                experiments_csv << furrycat_page.experiments.first.keys
                assemblies_csv << furrycat_page.assembly.keys
                samples_csv << furrycat_page.samples.first.keys
              end

              creatures_csv << furrycat_page.creature.values
              final_experiments_csv << furrycat_page.final_experiment.values
              assemblies_csv << furrycat_page.assembly.values

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
end

begin
  parse_files
rescue => exception
  puts exception
end
