require 'nokogiri'
require 'open-uri'

FILE_PATH = '../data/raw/Creatures.html'
DOWNLOAD_PATH = '../data/raw/furrycat'

html = File.open(FILE_PATH) do |f|
  Nokogiri::HTML(f)
end

html.css('a').each do |link|
  href = link['href']
  puts "Checking #{href}"
  next unless href.include?("creature.html?")

  filename = DOWNLOAD_PATH + "#{link.text}.html"

  begin
    content = URI.open(href).read

    File.open(filename, 'w') do |file|
      file.write(content)
    end

    puts "Downloaded #{href} to #{filename}"

  rescue OpenURI::HTTPError => e
    puts "Failed to download #{href}: #{e.message}"
  end
end
