require 'spec_helper'
require 'nokogiri'

require_relative '../furrycat_page'

RSpec.describe FurryCatPage do
  describe '#from_html' do
    context 'with 0etpojrb - bantha' do
      let :bantha do
        html =  File.open('../data/raw/furrycat/0etpojrb.html') { |f| Nokogiri::HTML(f) }
        FurryCatPage.from_html(html)
      end

      it 'should properly calculate all fields in the creature' do
        expect(bantha.creature[:serial]).to eq '0etpojrb'
        expect(bantha.creature[:skin]).to eq 'bantha'
        expect(bantha.creature[:level]).to eq 10
        expect(bantha.creature[:health]).to eq 9760
        expect(bantha.creature[:action]).to eq 5488
        expect(bantha.creature[:mind]).to eq 3281
        expect(bantha.creature[:speed]).to eq 2.13
        expect(bantha.creature[:to_hit]).to eq 0.23
        expect(bantha.creature[:damage_low]).to eq 130
        expect(bantha.creature[:damage_high]).to eq 140
        expect(bantha.creature[:armor]).to eq 0
        expect(bantha.creature['kinetic.special']).to eq 60
        expect(bantha.creature['kinetic.effective']).to eq 0
        expect(bantha.creature['energy.special']).to eq -64
        expect(bantha.creature['energy.effective']).to eq 0
        expect(bantha.creature['blast.special']).to eq 0
        expect(bantha.creature['blast.effective']).to eq 45
        expect(bantha.creature['heat.special']).to eq -34
        expect(bantha.creature['heat.effective']).to eq 0
        expect(bantha.creature['cold.special']).to eq -23
        expect(bantha.creature['cold.effective']).to eq 0
        expect(bantha.creature['electricity.special']).to eq -88
        expect(bantha.creature['electricity.effective']).to eq 0
        expect(bantha.creature['acid.special']).to eq -1
        expect(bantha.creature['acid.effective']).to eq 0
        expect(bantha.creature['stun.special']).to eq -99
        expect(bantha.creature['stun.effective']).to eq 0
        expect(bantha.creature[:sa1]).to eq ''
        expect(bantha.creature[:sa2]).to eq 'dizzy'
        expect(bantha.creature[:ranged]).to be false
      end

      it 'should calculate the template' do
        expect(bantha.template[:serial]).to eq '7t6nuj36'
        expect(bantha.template[:quality]).to eq 'above_average'
        expect(bantha.template[:hardiness]).to eq 581
        expect(bantha.template[:fortitude]).to eq 455
        expect(bantha.template[:dexterity]).to eq 344
        expect(bantha.template[:endurance]).to eq 298
        expect(bantha.template[:intellect]).to eq 100
        expect(bantha.template[:cleverness]).to eq 47
        expect(bantha.template[:dependability]).to eq 742
        expect(bantha.template[:courage]).to eq 365
        expect(bantha.template[:fierceness]).to eq 23
        expect(bantha.template[:power]).to eq 151
        expect(bantha.template[:armor]).to eq 0
        expect(bantha.template['kinetic.special']).to eq 60
        expect(bantha.template['kinetic.effective']).to eq 0
        expect(bantha.template['energy.special']).to eq -64
        expect(bantha.template['energy.effective']).to eq 0
        expect(bantha.template['blast.special']).to eq 0
        expect(bantha.template['blast.effective']).to eq 45
        expect(bantha.template['heat.special']).to eq -34
        expect(bantha.template['heat.effective']).to eq 0
        expect(bantha.template['cold.special']).to eq -23
        expect(bantha.template['cold.effective']).to eq 0
        expect(bantha.template['electricity.special']).to eq -88
        expect(bantha.template['electricity.effective']).to eq 0
        expect(bantha.template['acid.special']).to eq -1
        expect(bantha.template['acid.effective']).to eq 0
        expect(bantha.template['stun.special']).to eq -99
        expect(bantha.template['stun.effective']).to eq 0
        expect(bantha.template[:sa1]).to eq ''
        expect(bantha.template[:sa2]).to eq 'dizzy'
        expect(bantha.template[:ranged]).to be false
      end

      it 'should not find any experiments (this page has none)' do
        expect(bantha.experiments).to eq []
      end

      it 'should calculate all the samples' do
        phy = bantha.samples[0]
        pro = bantha.samples[1]
        men = bantha.samples[2]
        psy = bantha.samples[3]
        agg = bantha.samples[4]

        expect(phy[:source]).to eq 'merek_harvester'
        expect(phy[:generation]).to eq ''
        expect(phy[:quality]).to eq 'very_high'
        expect(phy[:hardiness]).to eq 721
        expect(phy[:fortitude]).to eq 619
        expect(phy[:dexterity]).to eq 681
        expect(phy[:endurance]).to eq 519
        expect(phy[:intellect]).to eq 744
        expect(phy[:cleverness]).to eq 555
        expect(phy[:dependability]).to eq 768
        expect(phy[:courage]).to eq 512
        expect(phy[:fierceness]).to eq 101
        expect(phy[:power]).to eq 663
        expect(phy[:armor]).to eq 1
        expect(phy['kinetic.special']).to eq 75
        expect(phy['kinetic.effective']).to eq 0
        expect(phy['energy.special']).to eq -99
        expect(phy['energy.effective']).to eq 0
        expect(phy['blast.special']).to eq 0
        expect(phy['blast.effective']).to eq 10
        expect(phy['heat.special']).to eq 0
        expect(phy['heat.effective']).to eq 10
        expect(phy['cold.special']).to eq 0
        expect(phy['cold.effective']).to eq 10
        expect(phy['electricity.special']).to eq -99
        expect(phy['electricity.effective']).to eq 0
        expect(phy['acid.special']).to eq 0
        expect(phy['acid.effective']).to eq 10
        expect(phy['stun.special']).to eq -99
        expect(phy['stun.effective']).to eq 0
        expect(phy[:sa1]).to eq 'area_combo'
        expect(phy[:sa2]).to eq 'dizzy'
        expect(phy[:ranged]).to be false

        expect(pro[:source]).to eq 'merek_harvester'
        expect(pro[:generation]).to eq ''
        expect(pro[:quality]).to eq 'high'
        expect(pro[:hardiness]).to eq 714
        expect(pro[:fortitude]).to eq 611
        expect(pro[:dexterity]).to eq 629
        expect(pro[:endurance]).to eq 511
        expect(pro[:intellect]).to eq 727
        expect(pro[:cleverness]).to eq 553
        expect(pro[:dependability]).to eq 757
        expect(pro[:courage]).to eq 509
        expect(pro[:fierceness]).to eq 96
        expect(pro[:power]).to eq 656
        expect(pro[:armor]).to eq 1
        expect(pro['kinetic.special']).to eq 75
        expect(pro['kinetic.effective']).to eq 0
        expect(pro['energy.special']).to eq -99
        expect(pro['energy.effective']).to eq 0
        expect(pro['blast.special']).to eq 0
        expect(pro['blast.effective']).to eq 10
        expect(pro['heat.special']).to eq 0
        expect(pro['heat.effective']).to eq 10
        expect(pro['cold.special']).to eq 0
        expect(pro['cold.effective']).to eq 10
        expect(pro['electricity.special']).to eq -99
        expect(pro['electricity.effective']).to eq 0
        expect(pro['acid.special']).to eq 0
        expect(pro['acid.effective']).to eq 10
        expect(pro['stun.special']).to eq -99
        expect(pro['stun.effective']).to eq 0
        expect(pro[:sa1]).to eq 'area_combo'
        expect(pro[:sa2]).to eq 'dizzy'
        expect(pro[:ranged]).to be false

        expect(men[:source]).to eq 'bearded_jax'
        expect(men[:generation]).to eq 'b3np596f'
        expect(men[:quality]).to eq 'above_average'
        expect(men[:hardiness]).to eq 310
        expect(men[:fortitude]).to eq 140
        expect(men[:dexterity]).to eq 251
        expect(men[:endurance]).to eq 400
        expect(men[:intellect]).to eq 167
        expect(men[:cleverness]).to eq 84
        expect(men[:dependability]).to eq 756
        expect(men[:courage]).to eq 342
        expect(men[:fierceness]).to eq 92
        expect(men[:power]).to eq 149
        expect(men[:armor]).to eq 0
        expect(men['kinetic.special']).to eq 44
        expect(men['kinetic.effective']).to eq 0
        expect(men['energy.special']).to eq 0
        expect(men['energy.effective']).to eq 0
        expect(men['blast.special']).to eq 0
        expect(men['blast.effective']).to eq 13
        expect(men['heat.special']).to eq -99
        expect(men['heat.effective']).to eq 0
        expect(men['cold.special']).to eq -68
        expect(men['cold.effective']).to eq 0
        expect(men['electricity.special']).to eq -68
        expect(men['electricity.effective']).to eq 0
        expect(men['acid.special']).to eq -5
        expect(men['acid.effective']).to eq 0
        expect(men['stun.special']).to eq -99
        expect(men['stun.effective']).to eq 0
        expect(men[:sa1]).to eq ''
        expect(men[:sa2]).to eq 'dizzy'
        expect(men[:ranged]).to be false

        expect(psy[:source]).to eq 'bearded_jax'
        expect(psy[:generation]).to eq 'b3np596f'
        expect(psy[:quality]).to eq 'above_average'
        expect(psy[:hardiness]).to eq 308
        expect(psy[:fortitude]).to eq 135
        expect(psy[:dexterity]).to eq 249
        expect(psy[:endurance]).to eq 399
        expect(psy[:intellect]).to eq 160
        expect(psy[:cleverness]).to eq 78
        expect(psy[:dependability]).to eq 750
        expect(psy[:courage]).to eq 343
        expect(psy[:fierceness]).to eq 93
        expect(psy[:power]).to eq 146
        expect(psy[:armor]).to eq 0
        expect(psy['kinetic.special']).to eq 44
        expect(psy['kinetic.effective']).to eq 0
        expect(psy['energy.special']).to eq 0
        expect(psy['energy.effective']).to eq 0
        expect(psy['blast.special']).to eq 0
        expect(psy['blast.effective']).to eq 13
        expect(psy['heat.special']).to eq -99
        expect(psy['heat.effective']).to eq 0
        expect(psy['cold.special']).to eq -68
        expect(psy['cold.effective']).to eq 0
        expect(psy['electricity.special']).to eq -68
        expect(psy['electricity.effective']).to eq 0
        expect(psy['acid.special']).to eq -5
        expect(psy['acid.effective']).to eq 0
        expect(psy['stun.special']).to eq -99
        expect(psy['stun.effective']).to eq 0
        expect(psy[:sa1]).to eq ''
        expect(psy[:sa2]).to eq 'dizzy'
        expect(psy[:ranged]).to be false

        expect(agg[:source]).to eq 'bearded_jax'
        expect(agg[:generation]).to eq 'b3np596f'
        expect(agg[:quality]).to eq 'above_average'
        expect(agg[:hardiness]).to eq 304
        expect(agg[:fortitude]).to eq 132
        expect(agg[:dexterity]).to eq 250
        expect(agg[:endurance]).to eq 404
        expect(agg[:intellect]).to eq 159
        expect(agg[:cleverness]).to eq 86
        expect(agg[:dependability]).to eq 755
        expect(agg[:courage]).to eq 342
        expect(agg[:fierceness]).to eq 90
        expect(agg[:power]).to eq 153
        expect(agg[:armor]).to eq 0
        expect(agg['kinetic.special']).to eq 44
        expect(agg['kinetic.effective']).to eq 0
        expect(agg['energy.special']).to eq 0
        expect(agg['energy.effective']).to eq 0
        expect(agg['blast.special']).to eq 0
        expect(agg['blast.effective']).to eq 13
        expect(agg['heat.special']).to eq -99
        expect(agg['heat.effective']).to eq 0
        expect(agg['cold.special']).to eq -68
        expect(agg['cold.effective']).to eq 0
        expect(agg['electricity.special']).to eq -68
        expect(agg['electricity.effective']).to eq 0
        expect(agg['acid.special']).to eq -5
        expect(agg['acid.effective']).to eq 0
        expect(agg['stun.special']).to eq -99
        expect(agg['stun.effective']).to eq 0
        expect(agg[:sa1]).to eq ''
        expect(agg[:sa2]).to eq 'dizzy'
        expect(agg[:ranged]).to be false
      end
    end

    context 'with m4ai12ck - woolamander' do
      let :woolamander do
        html =  File.open('../data/raw/furrycat/m4ai12ck.html') { |f| Nokogiri::HTML(f) }
        FurryCatPage.from_html(html)
      end

      it 'should properly calculate all fields in the creature' do
        expect(woolamander.creature[:serial]).to eq 'm4ai12ck'
        expect(woolamander.creature[:skin]).to eq 'woolamander'
        expect(woolamander.creature[:level]).to eq 40
        expect(woolamander.creature[:health]).to eq 11250
        expect(woolamander.creature[:action]).to eq 10245
        expect(woolamander.creature[:mind]).to eq 9671
        expect(woolamander.creature[:speed]).to eq 2.16
        expect(woolamander.creature[:to_hit]).to eq 0.44
        expect(woolamander.creature[:damage_low]).to eq 315
        expect(woolamander.creature[:damage_high]).to eq 340
        expect(woolamander.creature[:armor]).to eq 1
        expect(woolamander.creature['kinetic.special']).to eq 58
        expect(woolamander.creature['kinetic.effective']).to eq 0
        expect(woolamander.creature['energy.special']).to eq -6
        expect(woolamander.creature['energy.effective']).to eq 0
        expect(woolamander.creature['blast.special']).to eq 13
        expect(woolamander.creature['blast.effective']).to eq 0
        expect(woolamander.creature['heat.special']).to eq 28
        expect(woolamander.creature['heat.effective']).to eq 0
        expect(woolamander.creature['cold.special']).to eq 11
        expect(woolamander.creature['cold.effective']).to eq 0
        expect(woolamander.creature['electricity.special']).to eq -16
        expect(woolamander.creature['electricity.effective']).to eq 0
        expect(woolamander.creature['acid.special']).to eq 5
        expect(woolamander.creature['acid.effective']).to eq 0
        expect(woolamander.creature['stun.special']).to eq -29
        expect(woolamander.creature['stun.effective']).to eq 0
        expect(woolamander.creature[:sa1]).to eq ''
        expect(woolamander.creature[:sa2]).to eq 'crippling'
        expect(woolamander.creature[:ranged]).to be false
      end
    end
  end
end
