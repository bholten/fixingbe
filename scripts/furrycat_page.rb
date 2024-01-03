require 'securerandom'
require 'uri'
require 'pry'

require_relative 'str_utils'

class FurryCatPage
  attr_accessor :creature, :template, :experiments, :samples

  @@ABBREV = {
    'Phy' => :physique,
    'Pro' => :prowess,
    'Men' => :mental,
    'Psy' => :psychology,
    'Agg' => :aggression
  }

  @@QUALITY_MAP = {
    'low' => 0,
    'below_average' => 1,
    'average' => 2,
    'above_average' => 3,
    'high' => 4,
    'very_high' => 5
  }

  def initialize(args = {})
    @creature = args[:creature]
    @template = args[:template]
    @experiments = args[:experiments]
    @samples = args[:samples]
  end

  def self.parse_experiment_number_field(field_str)
    # 'Experiment 1: Agg: 3'
    roll_match = field_str.match(/Experiment (\d+)/)
    experiment_number = roll_match[1].to_i

    results = field_str.scan(/(\w+): (\d+)/)

    experiment_data = { experiment_number: experiment_number }

    results.each do |abbr, value|
      full_name = @@ABBREV[abbr]
      experiment_data[full_name] = value.to_i if full_name
    end

    experiment_data
  end


  def self.parse_resist(name, resist_element)

  end

  def self.get_swgemu_resist(element, start_idx, obj = {})
    resists = {}

    [:kinetic, :energy, :blast, :heat, :cold, :electricity, :acid, :stun].each.with_index do |resist, idx|
      resist_element = element[start_idx + idx]
      name = "#{resist}" # We leave the name in for easier CL calculations
      name_eff = "#{resist}.effective"
      name_spec = "#{resist}.special"
      res = resist_element.text.to_i
      part =
        if !resist_element.css('strong').empty? || res.negative?
          {name_spec => res, name_eff => 0}
        else
          {name_spec => 0, name_eff => res}
        end

      resists.merge!(part).merge!({name => res})
    end

    obj.merge(resists)
  end

  def self.from_html(html)
    # Get creature out of the first table
    creature_table = html.css('table:nth-of-type(1)')
    final_creature = creature_table.css('tr')[1].css('td')
    attacks = final_creature[18].text.split('/')

    # Get experiment/assembly table
    experiment_table = html.css('table:nth-of-type(2)')

    # The template is the last entry in the experiment table
    # Many pages do not have experiments, and just show the
    # final template
    template = experiment_table.css('tr')[1].css('td')

    # Creature args
    creature_args = {
      serial: final_creature[0].text,
      template_id: template[0].text,
      skin: StrUtils.to_snake_case(final_creature[1].text),
      level: final_creature[2].text.to_i,
      health: final_creature[3].text.to_i,
      action: final_creature[4].text.to_i,
      mind: final_creature[5].text.to_i,
      speed: final_creature[6].text.to_f,
      to_hit: final_creature[7].text.to_f,
      damage_low: final_creature[8].text.split('-')[0].strip.to_i,
      damage_high: final_creature[8].text.split('-')[1].strip.to_i,
      armor: StrUtils.is_light_armor(final_creature[9].text).to_i,
      sa1: if attacks[0] == '--' then '' else StrUtils.to_snake_case(attacks[0]) end,
      sa2: if attacks[1] == '--' then '' else StrUtils.to_snake_case(attacks[1]) end,
      ranged: attacks[2] != '--'
    }

    creature_args = get_swgemu_resist(final_creature, 10, creature_args)

    # Samples
    samples = html.css('table:nth-of-type(3)')

    sample_list = samples.css('tr')[1..-1].map do |row|
      sample = row.css('td')
      attacks = sample[21].text.split('/')

      s = {
        sample_id: SecureRandom.uuid,
        creature_serial: final_creature[0].text,
        source: StrUtils.to_snake_case(sample[0].text),
        generation: if sample[0].at_css('a') then
            URI(sample[0].at_css('a')['href']).query.split('=')[0]
          else
            ''
          end,
        quality: @@QUALITY_MAP[StrUtils.to_snake_case(sample[1].text)],
        hardiness: sample[2].text.to_i,
        fortitude: sample[3].text.to_i,
        dexterity: sample[4].text.to_i,
        endurance: sample[5].text.to_i,
        intellect: sample[6].text.to_i,
        cleverness: sample[7].text.to_i,
        dependability: sample[8].text.to_i,
        courage: sample[9].text.to_i,
        fierceness: sample[10].text.to_i,
        power: sample[11].text.to_i,
        armor: StrUtils.is_light_armor(sample[12].text),
        sa1: if attacks[0] == '--' then '' else StrUtils.to_snake_case(attacks[0]) end,
        sa2: if attacks[1] == '--' then '' else StrUtils.to_snake_case(attacks[1]) end,
        ranged: attacks[2] != '--'
      }
      get_swgemu_resist(sample, 13, s)
    end

    # Final Combine
    template_args = {
      id: SecureRandom.uuid,
      serial: template[0].text, ## changed
      quality: @@QUALITY_MAP[StrUtils.to_snake_case(template[1].text)],
      hardiness: template[2].text.to_i,
      fortitude: template[3].text.to_i,
      dexterity: template[4].text.to_i,
      endurance: template[5].text.to_i,
      intellect: template[6].text.to_i,
      cleverness: template[7].text.to_i,
      dependability: template[8].text.to_i,
      courage: template[9].text.to_i,
      fierceness: template[10].text.to_i,
      power: template[11].text.to_i,
      armor: StrUtils.is_light_armor(template[12].text),
      sa1: if attacks[0] == '--' then '' else StrUtils.to_snake_case(attacks[0]) end,
      sa2: if attacks[1] == '--' then '' else StrUtils.to_snake_case(attacks[1]) end,
      ranged: attacks[2] != '--',
      used: StrUtils.to_snake_case(template[22].text),
      physique: sample_list[0]&.[](:sample_id),
      prowess: sample_list[1]&.[](:sample_id),
      mental: sample_list[2]&.[](:sample_id),
      psychology: sample_list[3]&.[](:sample_id),
      aggression: sample_list[4]&.[](:sample_id)
    }

    template_args = get_swgemu_resist(template, 13, template_args)

    # Get experiments
    experiments = experiment_table.css('tr')[2...-1].map do |row|
      experiment = row.css('td')
      experiment_step = parse_experiment_number_field(experiment[13].text)

      experiment_args = {
        experiment_id: SecureRandom.uuid,
        serial: experiment[0].text, # changed
        quality: @@QUALITY_MAP[StrUtils.to_snake_case(experiment[1].text)],
        hardiness: experiment[2].text.to_i,
        fortitude: experiment[3].text.to_i,
        dexterity: experiment[4].text.to_i,
        endurance: experiment[5].text.to_i,
        intellect: experiment[6].text.to_i,
        cleverness: experiment[7].text.to_i,
        dependability: experiment[8].text.to_i,
        courage: experiment[9].text.to_i,
        fierceness: experiment[10].text.to_i,
        power: experiment[11].text.to_i,
        armor: StrUtils.is_light_armor(experiment[12].text),
        experiment_number: experiment_step[:experiment_number],
        assembly: experiment[13].text.strip.downcase == 'initial combine',
        physique: experiment_step.fetch(:physique, 0),
        prowess: experiment_step.fetch(:prowess, 0),
        mental: experiment_step.fetch(:mental, 0),
        psychology: experiment_step.fetch(:psychology, 0),
        aggression: experiment_step.fetch(:aggression, 0),
      }
    end

    FurryCatPage.new({
    creature: creature_args,
    template: template_args,
    experiments: experiments,
    samples: sample_list
    })
  end
end
