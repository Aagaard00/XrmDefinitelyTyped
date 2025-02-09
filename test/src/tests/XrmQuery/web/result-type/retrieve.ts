/// <reference path='../../../../../typings/XRM/dg.xrmquery.web.d.ts'/>
import { expect } from 'chai';
import { suite, test, slow, timeout, skip, only } from "@testdeck/mocha";

@suite
class Web_Retrieve_ResultTypeCheck {

    contactId: string;
    responseId: string;

    before() {
        this.contactId = "CONTACT_ID";
        this.responseId = "RESPONSE_ID";
    }

    @test
    "no select, without formatted"() {
        XrmQuery.retrieve(x => x.contacts, this.contactId)
            .execute(x => x.contactid)
    }

    @test
    "no select, with formatted"() {
        XrmQuery.retrieve(x => x.contacts, this.contactId)
            .includeFormattedValues()
            .execute(x => x.birthdate_formatted)
    }

    @test
    "select, without formatted"() {
        XrmQuery.retrieve(x => x.contacts, this.contactId)
            .select(x => [x.birthdate])
            .execute(x => x.birthdate)
    }

    @test
    "select, with formatted"() {
        XrmQuery.retrieve(x => x.contacts, this.contactId)
            .select(x => [x.birthdate])
            .includeFormattedValues()
            .execute(x => x.birthdate_formatted)
    }

    @test
    "select, attribute name matches entity name"() {
        XrmQuery.retrieve(x => x.dg_responses, this.responseId)
            .select(x => [x.dg_response])
            .execute(x => x.dg_response)
    }
}