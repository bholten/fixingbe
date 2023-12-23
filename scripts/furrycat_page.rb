require 'securerandom'
require 'uri'

require_relative 'str_utils'

class FurryCatPage
  attr_accessor :creature, :final_experiment, :assembly,
                :experiments, :samples

  @@ABBREV = {
    'Phy' => :physique,
    'Pro' => :prowess,
    'Men' => :mental,
    'Psy' => :psychology,
    'Agg' => :aggression
  }

  def initialize(args = {})
    @creature = args[:creature]
    @final_experiment = args[:final_experiment]
    @assembly = args[:assembly]
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

  def self.from_html(html)
    # Get creature out of the first table
    creature_table = html.css('table:nth-of-type(1)')
    final_creature = creature_table.css('tr')[1].css('td')
    attacks = final_creature[18].text.split('/')

    # Get experiment/assembly table
    experiment_table = html.css('table:nth-of-type(2)')

    # Final experiment for ID to reference in Creature table
    final_experiment = experiment_table.css('tr')[1].css('td')

    # Creature args
    creature_args = {
      creature_id: final_creature[0].text,
      final_experimentation_id: final_experiment[0].text,
      skin: final_creature[1].text,
      creature_level: final_creature[2].text,
      health: final_creature[3].text,
      action: final_creature[4].text,
      mind: final_creature[5].text,
      speed: final_creature[6].text,
      to_hit: final_creature[7].text,
      damage_low: final_creature[8].text.split('-')[0].strip,
      damage_high: final_creature[8].text.split('-')[1].strip,
      armor: StrUtils.to_snake_case(final_creature[9].text),
      kinetic: final_creature[10].text,
      energy: final_creature[11].text,
      blast: final_creature[12].text,
      heat: final_creature[13].text,
      cold: final_creature[14].text,
      electricity: final_creature[15].text,
      acid: final_creature[16].text,
      stun: final_creature[17].text,
      sa1: if attacks[0] == '--' then '' else StrUtils.to_snake_case(attacks[0]) end,
      sa2: if attacks[1] == '--' then '' else StrUtils.to_snake_case(attacks[1]) end,
      ranged: attacks[2] != '--'
    }

    # @creature = Creature.new(creature_args)
    puts creature_args

    # Final Combine
    final_experiment_args = {
      id: SecureRandom.uuid,
      serial: final_experiment[0].text, ## changed
      quality: StrUtils.to_snake_case(final_experiment[1].text),
      hardiness: final_experiment[2].text,
      fortitude: final_experiment[3].text,
      dexterity: final_experiment[4].text,
      endurance: final_experiment[5].text,
      intellect: final_experiment[6].text,
      cleverness: final_experiment[7].text,
      dependability: final_experiment[8].text,
      courage: final_experiment[9].text,
      fierceness: final_experiment[10].text,
      power: final_experiment[11].text,
      armor: StrUtils.to_snake_case(final_experiment[12].text),
      kinetic: final_experiment[13].text,
      energy: final_experiment[14].text,
      blast: final_experiment[15].text,
      heat: final_experiment[16].text,
      cold: final_experiment[17].text,
      electricity: final_experiment[18].text,
      acid: final_experiment[19].text,
      stun: final_experiment[21].text,
      sa1: if attacks[0] == '--' then '' else StrUtils.to_snake_case(attacks[0]) end,
      sa2: if attacks[1] == '--' then '' else StrUtils.to_snake_case(attacks[1]) end,
      ranged: attacks[2] != '--',
      used: StrUtils.to_snake_case(final_experiment[22].text)
    }

    # @final_combine = FinalCombine.new(final_experiment_args)
    puts final_experiment_args

    # Get experiments
    experiments = experiment_table.css('tr')[2...-1].map do |row|
      experiment = row.css('td')
      experiment_step = parse_experiment_number_field(experiment[13].text)

      experiment_args = {
        experiment_id: SecureRandom.uuid,
        serial: experiment[0].text, # changed
        quality: StrUtils.to_snake_case(experiment[1].text),
        hardiness: experiment[2].text,
        fortitude: experiment[3].text,
        dexterity: experiment[4].text,
        endurance: experiment[5].text,
        intellect: experiment[6].text,
        cleverness: experiment[7].text,
        dependability: experiment[8].text,
        courage: experiment[9].text,
        fierceness: experiment[10].text,
        power: experiment[11].text,
        armor: StrUtils.to_snake_case(experiment[12].text),
        experiment_number: experiment_step[:experiment_number],
        physique: experiment_step.fetch(:physique, 0),
        prowess: experiment_step.fetch(:prowess, 0),
        mental: experiment_step.fetch(:mental, 0),
        psychology: experiment_step.fetch(:psychology, 0),
        aggression: experiment_step.fetch(:aggression, 0),
      }
    end

    puts experiments

    # Samples
    samples = html.css('table:nth-of-type(3)')

    sample_list = samples.css('tr')[1..-1].map do |row|
      sample = row.css('td')
      attacks = sample[21].text.split('/')

      {
        sample_id: SecureRandom.uuid,
        source: StrUtils.to_snake_case(sample[0].text),
        generation: if sample[0].at_css('a') then
            URI(sample[0].at_css('a')['href']).query.split('=')[0]
          else
            ''
          end,
        quality: StrUtils.to_snake_case(sample[1].text),
        hardiness: sample[2].text,
        fortitude: sample[3].text,
        dexterity: sample[4].text,
        endurance: sample[5].text,
        intellect: sample[6].text,
        cleverness: sample[7].text,
        dependability: sample[8].text,
        courage: sample[9].text,
        fierceness: sample[10].text,
        power: sample[11].text,
        armor: StrUtils.to_snake_case(sample[12].text),
        kinetic: sample[13].text,
        energy: sample[14].text,
        blast: sample[15].text,
        heat: sample[16].text,
        cold: sample[17].text,
        electricity: sample[18].text,
        acid: sample[19].text,
        stun: sample[20].text,
        sa1: if attacks[0] == '--' then '' else StrUtils.to_snake_case(attacks[0]) end,
        sa2: if attacks[1] == '--' then '' else StrUtils.to_snake_case(attacks[1]) end,
        ranged: attacks[2] != '--'
      }
    end

    puts sample_list

    # Assembly
    assembly_row = experiment_table.css('tr')[-1].css('td')

    assembly_args = {
        experiment_id: SecureRandom.uuid,
        serial: assembly_row[0].text, # changed
        quality: StrUtils.to_snake_case(assembly_row[1].text),
        hardiness: assembly_row[2].text,
        fortitude: assembly_row[3].text,
        dexterity: assembly_row[4].text,
        endurance: assembly_row[5].text,
        intellect: assembly_row[6].text,
        cleverness: assembly_row[7].text,
        dependability: assembly_row[8].text,
        courage: assembly_row[9].text,
        fierceness: assembly_row[10].text,
        power: assembly_row[11].text,
        armor: StrUtils.to_snake_case(assembly_row[12].text),
        # Note that data without samples is invalid for us
        # However, Furrycat's dataset does indeed have some
        # pages without samples -- just the final combine
        physique: sample_list[0]&.[](:sample_id),
        prowess: sample_list[1]&.[](:sample_id),
        mental: sample_list[2]&.[](:sample_id),
        psychology: sample_list[3]&.[](:sample_id),
        aggression: sample_list[4]&.[](:sample_id)
      }

    puts assembly_args

    FurryCatPage.new({
    creature: creature_args,
    final_experiment: final_experiment_args,
    assembly: assembly_args,
    experiments: experiments,
    samples: sample_list
    })
  end
end
