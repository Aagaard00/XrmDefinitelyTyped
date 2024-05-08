/// <reference path='../../../../../typings/XRM/dg.xrmquery.web.d.ts'/>
import { expect } from 'chai';
import { suite, test, slow, timeout, skip, only } from "@testdeck/mocha";

@suite
class Web_RetrieveMultiple_ResultTypeCheck {


    @test
    "no select, without formatted"() {
        XrmQuery.retrieveMultiple(x => x.contacts)
            .execute(x => x[0].contactid)
    }

    @test
    "no select, with formatted"() {
        XrmQuery.retrieveMultiple(x => x.contacts)
            .includeFormattedValues()
            .execute(x => x[0].birthdate_formatted)
    }

    @test
    "select, without formatted"() {
        XrmQuery.retrieveMultiple(x => x.contacts)
            .select(x => [x.birthdate])
            .execute(x => x[0].birthdate)
    }

    @test
    "select, with formatted"() {
        XrmQuery.retrieveMultiple(x => x.contacts)
            .select(x => [x.birthdate])
            .includeFormattedValues()
            .execute(x => x[0].birthdate_formatted)
    }

    @test
    "select, attribute name matches entity name"() {
        XrmQuery.retrieveMultiple(x => x.dg_responses)
            .select(x => [x.dg_response])
            .execute(x => x[0].dg_response)
    }

}